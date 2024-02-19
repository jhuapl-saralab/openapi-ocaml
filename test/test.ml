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
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;;

type poly_var = [`X of int | `Y of string]
[@@deriving yojson,json_schema];;

type test_var = A of int | B of string | C of int * string * float
[@@deriving yojson,json_schema];;

type test_rec = {
  x : int;
  y : string;
  z : string option [@yojson.option];
  w : string list;
  a : test_var;
  p : poly_var
} [@@deriving yojson,json_schema {defs = "#/$defs"}]

let derived_schema =
  let open Openapi.Json_schema.Helpers in
  test_rec_schema
  |> def "test_var_schema" test_var_schema
  |> def "poly_var_schema" poly_var_schema
    
let () =
  let open Openapi.Json_schema.Helpers in
  let explicit_schema = object_
                        |> id_ "test_rec_schema"
                        |> title "test_rec"
                        |> property ~required:true "x" (obj integer)
                        |> property ~required:true "y" (obj string)
                        |> property ~required:false "z" (obj string)
                        |> property ~required:true "w" (array
                                                        |> items (obj string)
                                                        |> obj)
                        |> property ~required:true "a" (ref "#/$defs/test_var_schema")
                        |> property ~required:true "p" (ref "#/$defs/poly_var_schema")
                        |> def "test_var_schema"
                          (empty
                           |> one_of [array
                                      |> prefix_items [obj (empty |> const (`String "A"));
                                                       obj integer]
                                      |> max_items 2
                                      |> obj;
                                      array
                                      |> prefix_items [obj (empty |> const (`String "B"));
                                                       obj string]
                                      |> max_items 2
                                      |> obj;
                                      array
                                      |> prefix_items [obj (empty |> const (`String "C"));
                                                       obj integer;
                                                       obj string;
                                                       obj number]
                                      |> max_items 4
                                      |> obj]
                           |> id_ "test_var_schema"
                           |> title "test_var")
                        |> def "poly_var_schema"
                          (empty
                           |> one_of [empty
                                      |> one_of [array
                                                 |> prefix_items [obj (string |> const (`String "X")); obj integer]
                                                 |> max_items 2
                                                 |> obj;
                                                 array
                                                 |> prefix_items [obj (string |> const (`String "Y")); obj string]
                                                 |> max_items 2
                                                 |> obj]
                                      |> obj]
                           |> id_ "poly_var_schema"
                           |> title "poly_var")
  in
  if Openapi.Json_schema.equal_schema derived_schema explicit_schema
  then ()
  else
    let to_s x = Openapi.Json_schema.yojson_of_schema x
                 |> Yojson.Safe.pretty_to_string in
    Printf.sprintf
      "Schemas are not equal:\nderived: %s\nexplicit: %s\n"
      (to_s derived_schema) (to_s explicit_schema)
    |> failwith

let () =
  derived_schema
  |> Openapi.Json_schema.yojson_of_schema
  |> Yojson.Safe.pretty_to_string
  |> fun s ->
  Out_channel.with_file "test-schema.json"
    ~f:(fun chan -> Out_channel.output_string chan s)


let () =
  let obj = {x = 1; y = "hello"; z = None; w = ["a"; "b"; "c"]; a = C (5, "c_str", 1.3); p = `X 5} in
  yojson_of_test_rec obj
  |> Yojson.Safe.pretty_to_string
  |> fun s ->
  Out_channel.with_file "test-obj.json"
    ~f:(fun chan -> Out_channel.output_string chan s)

