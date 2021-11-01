open Core;;

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
    | Integer;;

let string_of_json_schema_type = function
    | Null -> "null"
    | Boolean -> "boolean"
    | Object -> "object"
    | Array -> "array"
    | Number -> "number"
    | String -> "string"
    | Integer -> "integer";;

let json_schema_type_of_yojson : Yojson.Safe.t -> json_schema_type = function
    | `String "null"    -> Null
    | `String "boolean" -> Boolean
    | `String "object"  -> Object
    | `String "array"   -> Array
    | `String "number"  -> Number
    | `String "string"  -> String
    | `String "integer" -> Integer
    | x -> raise (Yojson_conv.Of_yojson_error (Failure "unexpected input", x))
                       
let yojson_of_json_schema_type : json_schema_type -> Yojson.Safe.t  =
  fun x -> `String (string_of_json_schema_type x)
               
let pp_json_schema_type fmt x = Yojson.Safe.pp fmt (yojson_of_json_schema_type x);;

type schema = {
  schema      : string option [@key "$schema"] [@yojson.optional];
  id_         : string option [@key "$id"]  [@yojson.optional];
  title       : string option [@yojson.optional];
  description : string option [@yojson.optional];
  typ         : json_schema_type or_ref option [@key "type"] [@yojson.optional];
  enum        : any list option [@yojson.optional];
  const       : any option [@yojson.optional];    
  properties  : schema map option [@yojson.optional];
  items       : schema or_ref option [@yojson.optional];
  prefix_items : schema or_ref list option [@key "prefixItems"]
                 [@yojson.optional];
  format      : string option [@yojson.optional];
  any_of      : schema or_ref list option [@key "anyOf"] [@yojson.optional]
} [@@deriving make,show,yojson]
[@@yojson.allow_extra_fields]
;;

module Helpers = struct
  let obj o = Obj o;;
  let ref r = Ref r;;

  let empty    = make_schema ();;
  let title t s = {s with title = Some t};;
  let description d s = {s with description = Some d};;
  let const d s = {s with const = Some d};;
  let typ t s  = {s with typ = Some t};;
  let format f s = {s with format = Some f};;
  let items is s = {s with items = Some is};;
  let prefix_items pis s = {s with prefix_items = Some pis};;
  let enum xs s  = {s with enum  = Some xs};;
  let any_of xs s = {s with any_of = Some xs};;
  let properties ps s = {s with properties = Some ps};;
  let property k v s = {s with properties = Option.value ~default:[] s.properties
                                            |> (fun ps -> ps@[k,v])
                                            |> Option.return};;
  let null          = empty |> typ (Obj Null);;
  let boolean       = empty |> typ (Obj Boolean);;
  let object_       = empty |> typ (Obj Object);;
  let array         = empty |> typ (Obj Array);;
  let number        = empty |> typ (Obj Number);;
  let string        = empty |> typ (Obj String);;
  let integer       = empty |> typ (Obj Integer);;

  let array_of s = array |> items s;;

  let datetime      = string |> format "date-time";;
  let time          = string |> format "time";;
  let date          = string |> format "date";;
  let email         = string |> format "email";;
  let idn_email     = string |> format "idn-email";;
  let hostname      = string |> format "hostname";;
  let idn_hostname  = string |> format "idn-hostname";;
  let ipv4          = string |> format "ipv4";;
  let ipv6          = string |> format "ipv6";;
  let uri           = string |> format "uri";;
  let uri_reference = string |> format "uri-reference";;
  let iri           = string |> format "iri";;
  let iri_reference = string |> format "iri-reference";;
end;;
