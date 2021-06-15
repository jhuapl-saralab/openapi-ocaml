open Core;;
open Base.Poly;;

module Yojson_conv = Ppx_yojson_conv_lib.Yojson_conv;;

type 'a map = (string * 'a) list [@@deriving show];;

let yojson_of_map f xs : Yojson.Safe.t =
  `Assoc (List.map ~f:(fun (k,v) -> (k, f v)) xs);;

let map_of_yojson (f : Yojson.Safe.t -> 'a) : Yojson.Safe.t ->'a map = function  
  | `Assoc xs -> List.map ~f:(fun (k,v) -> (k, (f v))) xs
  | x -> raise (Yojson_conv.Of_yojson_error (Failure "unexpected input", x))

type any = Yojson.Safe.t;;
let pp_any = Yojson.Safe.pp;;
let any_of_yojson x = x;;
let yojson_of_any x = x;;

type any_map = any map [@@deriving show];;
let any_map_of_yojson = map_of_yojson (fun v -> v);; (* schema_object_of_yojson *);;
let yojson_of_any_map = yojson_of_map (fun v -> v);; (* yojson_of_schema_object *);;


type 'a or_ref = Obj of 'a | Ref of string;;

let yojson_of_or_ref f = function
  | Obj x -> f x
  | Ref r -> `Assoc ["$ref", `String r];;

let or_ref_of_yojson f = function
  | `Assoc ["$ref", `String x] -> Ref x
  | j -> Obj (f j)

let pp_or_ref (f:Format.formatter -> 'a -> unit) pp : ('a or_ref) -> unit = function
    | Obj x -> f pp x
    | Ref r -> Format.fprintf pp "[$ref %s]" r
                   
type json_schema_type =
    | Null
    | Boolean
    | Object
    | Array
    | Number
    | String
    | Integer
    | TypeArray of string list;;

let string_of_json_schema_type = function
    | Null -> "null"
    | Boolean -> "boolean"
    | Object -> "object"
    | Array -> "array"
    | Number -> "number"
    | String -> "string"
    | Integer -> "integer"
    | TypeArray ts -> sprintf "[%s]" (String.concat ~sep:", " ts)

let json_schema_type_of_yojson : Yojson.Safe.t -> json_schema_type = function
    | `String "null"    -> Null
    | `String "boolean" -> Boolean
    | `String "object"  -> Object
    | `String "array"   -> Array
    | `String "number"  -> Number
    | `String "string"  -> String
    | `String "integer" -> Integer
    | `List xs          -> TypeArray
                               (List.map ~f:(function
                                                 | `String x -> x
                                                 | x         -> raise (Yojson_conv.Of_yojson_error (Failure "unexpected input", x)))
                                      xs)
    | x -> raise (Yojson_conv.Of_yojson_error (Failure "unexpected input", x))
                       
let yojson_of_json_schema_type : json_schema_type -> Yojson.Safe.t = function
    | TypeArray ts -> `List (List.map ~f:(fun s -> `String s) ts)
    | x -> `String (string_of_json_schema_type x)
               
let pp_json_schema_type fmt x = Yojson.Safe.pp fmt (yojson_of_json_schema_type x);;
 
type schema = {
    schema : string option
             [@key "$schema"] [@default None]
             [@yojson_drop_default (=)];
    _id : string [@key "$id"];
    title : string option
            [@default None] [@yojson_drop_default (=)];
    description : string option
                  [@default None] [@yojson_drop_default (=)];
    typ : json_schema_type or_ref option
          [@key "type"] [@default None]
          [@yojson_drop_default (=)];
    enum : any list option
           [@default None] [@yojson_drop_default (=)];
    const : any option
            [@default None] [@yojson_drop_default (=)];    
    properties : schema map option
                 [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson]
[@@yojson.allow_extra_fields]
;;
