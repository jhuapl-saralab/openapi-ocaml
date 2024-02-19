(*
Copyright 2024 Johns Hopkins University Applied Physics Laboratory

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Core;;
open Ppxlib;;

type schema_settings = {
  defs : string [@default "#/$defs"]
} [@@deriving make];;

let schema_ocaml_name_for_type_name s = if String.equal s "t" then "schema" else sprintf "%s_schema" s
let schema_id_for_type_name s = if String.equal s "t" then "schema" else sprintf "%s_schema" s
let schema_title_for_type_name s = if String.equal s "t" then "schema" else s
    
let schema_ref_for_type_name ~settings ~loc s =
  let open Ast_builder.Default in
  schema_id_for_type_name s
  |> estring ~loc
  |> fun nm ->
  [%expr
    Printf.sprintf "%s/%s"
      [%e estring ~loc settings.defs]
      [%e nm]
    |> ref]

let rec schema_longident_for_longident =
  function
  | Lident s -> Lident (schema_ocaml_name_for_type_name s)
  | Ldot (li,s) -> Ldot (li, (schema_ocaml_name_for_type_name s))
  | Lapply (l,r) -> Lapply (l, schema_longident_for_longident r)

let schema_ocaml_val_name_for_longident ~loc li =
  Ast_builder.Default.pexp_ident ~loc {txt = schema_longident_for_longident li;
                                       loc = {loc with loc_ghost = true}}

let rec schema_ref_for_longident ~settings ~loc = function
  | Lident s     -> schema_ref_for_type_name ~settings ~loc s
  | Ldot (_li,s) -> schema_ref_for_type_name ~settings ~loc s
  | Lapply (_,l) -> schema_ref_for_longident ~settings ~loc l

let get_attribute_payload name =
  List.find_map ~f:(function
      | {attr_name = {txt;_}; attr_payload = PStr [%str [%e? ast]];_} ->
        if String.equal txt name then Some ast else None
      | _ -> None)

let has_attribute name =
  List.exists ~f:(function
      | {attr_name = {txt;_};_} when String.equal txt name -> true
      | _ -> false)

let inline_schema_attr = "inline_schema";;

let rec schema_for_core_type ?(json_option = false) ~settings typ =
  let loc = typ.ptyp_loc in
  match typ with
  | {ptyp_desc = Ptyp_alias (typ,s);ptyp_attributes;_} ->
    if has_attribute inline_schema_attr ptyp_attributes
    then [%expr [%e schema_for_core_type ~settings typ] |> obj]
    else schema_ref_for_type_name ~settings ~loc s
  | {ptyp_desc = Ptyp_constr (c,[]); ptyp_attributes;_} -> (
      let loc = c.loc in
      match c.txt with
      | Lident "int"    -> [%expr obj integer]
      | Lident "float"  -> [%expr obj number]
      | Lident "string" -> [%expr obj string]
      | Lident "bool"   -> [%expr obj boolean]
      | Lident "any"    -> [%expr obj empty]
      | x               -> (if has_attribute inline_schema_attr ptyp_attributes
                            then [%expr [%e schema_ocaml_val_name_for_longident ~loc x] |> obj]
                            else schema_ref_for_longident ~settings ~loc x)
    )
  | {ptyp_desc = Ptyp_constr (c,arg::[]);_} -> (match c.txt with
      | Lident "option" -> (if json_option
                            then schema_for_core_type ~settings arg
                            else [%expr empty
                                  |> one_of [obj null;
                                             [%e schema_for_core_type ~settings arg]]
                                  |> obj])
      | Lident "list"   -> [%expr array
                                  |> items [%e schema_for_core_type ~settings arg]
                                  |> obj]
      | Lident "map"    -> [%expr object_
                                  |> additional_properties [%e schema_for_core_type ~settings arg]
                                  |> obj]
      | x -> ([%expr [%e schema_ocaml_val_name_for_longident ~loc x]
                 [%e schema_for_core_type ~settings arg]]))
  | {ptyp_desc = Ptyp_constr (x,args);_}  -> (
      let open Ast_builder.Default in
      pexp_apply ~loc (schema_ocaml_val_name_for_longident ~loc:x.loc x.txt)
        (List.map ~f:(fun targ -> (Nolabel, schema_for_core_type ~settings targ)) args))
  | {ptyp_desc = Ptyp_tuple typs; _} ->
    let open Ast_builder.Default in
    let xs = List.map ~f:(schema_for_core_type ~settings) typs in
    let len = List.length xs in
    [%expr array
           |> prefix_items [%e elist ~loc xs]
           |> max_items [%e eint ~loc len]
           |> obj]
  | {ptyp_desc = Ptyp_variant (rfs,_closed,_xs); ptyp_loc = loc; _} ->
    let variants = List.map ~f:(schema_for_row_field ~settings) rfs
                   |> Ast_builder.Default.elist ~loc in
    [%expr empty |> one_of [%e variants] |> obj]
  (* fixme: attach a warning for ignored *)
  | {ptyp_desc = Ptyp_any ; _}        -> [%expr [%ocaml.error "unable to create schema for any type"]]
  | {ptyp_desc = Ptyp_var _; _}       -> [%expr [%ocaml.error "unable to create schema for var type"]]
  | {ptyp_desc = Ptyp_arrow _; _}     -> [%expr [%ocaml.error "unable to create schema for arrow type"]]
  | {ptyp_desc = Ptyp_class _; _}     -> [%expr [%ocaml.error "unable to create schema for class type"]]
  | {ptyp_desc = Ptyp_object _; _}    -> [%expr [%ocaml.error "unable to create schema for object type"]]
  | {ptyp_desc = Ptyp_poly _; _}      -> [%expr [%ocaml.error "unable to create schema for poly type"]]
  | {ptyp_desc = Ptyp_package _; _}   -> [%expr [%ocaml.error "unable to create schema for package type"]]
  | {ptyp_desc = Ptyp_extension _; _} -> [%expr [%ocaml.error "unable to create schema for extension type"]]
and schema_for_row_field ~settings {prf_desc; prf_attributes; _} =
  match prf_desc with
  | Rtag ({txt;loc}, _, typs) ->
    let open Ast_builder.Default in
    let name = get_attribute_payload "name" prf_attributes
               |> Option.value ~default:(estring ~loc txt) in
    let constr_schema = [%expr (string |> const (`String [%e name]) |> obj)] in
    let typ_schemas = elist ~loc (List.map ~f:(fun t -> schema_for_core_type ~settings t) typs) in
    let len = (List.length typs)+1 in
    [%expr array
           |> prefix_items ([%e constr_schema]::[%e typ_schemas])
           |> max_items [%e eint ~loc len]
           |> obj]
  | Rinherit x -> schema_for_core_type ~settings x
      
let gen_properties ~settings flds schema =
  List.fold_left flds ~init:schema
    ~f:(fun schema (ld : label_declaration) ->
        let open Ast_builder.Default in
        let loc = ld.pld_loc in
        let json_option = has_attribute "yojson.option" ld.pld_attributes in
        let has_default = (has_attribute "default" ld.pld_attributes ||
                           has_attribute "yojson_drop_default" ld.pld_attributes ||
                           has_attribute "yojson_drop_default.compare" ld.pld_attributes ||
                           has_attribute "yojson_drop_default.equal" ld.pld_attributes ||
                           has_attribute "yojson_drop_default.yojson" ld.pld_attributes ||
                           has_attribute "yojson_drop_if" ld.pld_attributes) in
        let required = (not has_default) && (not json_option) in
        let fld_typ = schema_for_core_type ~json_option ~settings ld.pld_type in
        let key = get_attribute_payload "key" ld.pld_attributes
                  |> Option.value ~default:(estring ~loc ld.pld_name.txt) in
        [%expr [%e schema]
               |> property ~required:[%e ebool ~loc required]
                 ([%e key]) [%e fld_typ]])
                                                           
let schema_for_constructor ~settings (c : constructor_declaration) =
  let loc = c.pcd_loc in
  let open Ast_builder.Default in
  let nm = get_attribute_payload "name" c.pcd_attributes
           |> Option.value ~default:(estring ~loc:c.pcd_name.loc c.pcd_name.txt) in
  (match c.pcd_args with
   | Pcstr_tuple typs -> List.map ~f:(schema_for_core_type ~settings) typs
   | Pcstr_record lds -> List.map ~f:(fun ld -> schema_for_core_type ~settings ld.pld_type) lds)
  |> fun its ->
  let len = (List.length its)+1 in
  [%expr array
         |> prefix_items ((empty |> const (`String [%e nm]) |> obj)::
                          [%e elist ~loc its])
         |> max_items [%e eint ~loc len]]
                          
let generate_impl ~ctxt:_ (_rec, tds) defs title id =
  List.map ~f:(fun (td : type_declaration) ->
      let open Ast_builder.Default in
      let loc         = td.ptype_loc in
      let settings    = make_schema_settings ?defs () in 
      let schema_id   = id |> Option.value ~default:(schema_id_for_type_name td.ptype_name.txt) in
      let title       = title |> Option.value ~default:(schema_title_for_type_name td.ptype_name.txt) in
      let schema      = [%expr empty] in
      let schema      = [%expr [%e schema]
                               |> id_   [%e estring ~loc schema_id]
                               |> title [%e estring ~loc title]] in
      let schema = match td.ptype_kind with
        | Ptype_abstract     ->
          (match td.ptype_manifest with
           | Some typ   -> [%expr [%e schema]
                                  |> one_of [[%e schema_for_core_type ~settings typ]]]
           | None       -> schema)
        | Ptype_variant cs   -> (let cs = List.map ~f:(fun c -> [%expr obj
                                                          [%e schema_for_constructor
                                                              ~settings c]]) cs in
                                 [%expr [%e schema] |> one_of [%e elist ~loc cs]])
        | Ptype_open         -> schema
        | Ptype_record flds  -> [%expr [%e schema]
                                       |> typ (obj Openapi.Json_schema.Object)]
                                |> gen_properties ~settings flds
      in
      [%stri let [%p pvar ~loc (schema_ocaml_name_for_type_name td.ptype_name.txt)] =
               let open Openapi.Json_schema.Helpers in
               [%e schema]]
    ) tds

let generate_intf ~ctxt:_ (_rec, (tds : type_declaration list)) =
  let gen_sigi (td : type_declaration) =
    let loc   = td.ptype_loc in
    let type_ = [%type: Openapi.Json_schema.t] in
    let name = schema_ocaml_name_for_type_name td.ptype_name.txt
               |> fun txt -> {loc; txt} in
    let value_description = (Ast_builder.Default.value_description ~loc
                               ~name ~type_ ~prim:[]) in
    Ast_builder.Default.psig_value ~loc value_description
  in
  List.map ~f:gen_sigi tds;;
             
let schema =
  let sig_type_decl = Deriving.Generator.V2.make_noarg generate_intf in
  let args = Deriving.Args.(empty
                            +> arg "defs" (estring __)
                            +> arg "title" (estring __)
                            +> arg "id" (estring __)) in
  let str_type_decl = Deriving.Generator.V2.make args generate_impl in
    Deriving.add
    ~str_type_decl
    ~sig_type_decl
    "json_schema"
