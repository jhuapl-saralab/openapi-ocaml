open Core;;
open Base.Poly;;
open Json_schema;;

type contact_object = {
  name  : string option [@default None] [@yojson_drop_default (=)];
  url   : string option [@default None] [@yojson_drop_default (=)];
  email : string option [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type license_object = {
    name : string;
    url  : string option [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;


type server_variable_object = {
  enum        : string list option [@default None] [@yojson_drop_default (=)];
  default     : string;
  description : string option [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type external_documentation_object = {
  description : string option [@default None] [@yojson_drop_default (=)];
  url         : string;
} [@@deriving make,show,yojson];;

type server_object = {
    url         : string;
    description : string option [@default None] [@yojson_drop_default (=)];
    variables   : server_variable_object map option
                  [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;

type link_object = {
    operation_ref : string option [@key "operationRef"]
                    [@default None] [@yojson_drop_default (=)];
    operation_id  : string option [@key "operationId"]
                    [@default None] [@yojson_drop_default (=)];
    parameters    : any map option [@default None]
                    [@yojson_drop_default (=)];
    request_body  : any  option [@key "requestBody"]
                    [@default None] [@yojson_drop_default (=)]; 
    description   : string option [@default None]
                    [@yojson_drop_default (=)];
    server        : server_object option [@default None]
                    [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type header_object = {
  description       : string option
                      [@default None] [@yojson_drop_default (=)];
  required          : bool option
                      [@default None] [@yojson_drop_default (=)];
  deprecated        : bool option
                      [@default None] [@yojson_drop_default (=)];
  allow_empty_value : bool option [@key "allowEmptyValue"]
                      [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type response_object = {
  description : string;
  headers     : header_object or_ref map option
                [@default None] [@yojson_drop_default (=)];
  link        : link_object or_ref map option
                [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type responses_object = response_object map [@@deriving show, yojson];;

type parameter_location = Query | Header | Path | Cookie [@@deriving show,yojson];;
let parameter_location_of_yojson = function
  | `String "query"   -> Query
  | `String "header"  -> Header
  | `String "path"    -> Path
  | `String "cookie" -> Cookie
  | x -> let open Ppx_yojson_conv_lib.Yojson_conv in    
    raise (Of_yojson_error
             (Failure (sprintf "%s: unexpected value must be \"query\", \"header\", \"path\", or \"cookie\"" __LOC__), x));;

let yojson_of_parameter_location = function
  | Query  -> `String "query"
  | Header -> `String "header"
  | Path   -> `String "path"
  | Cookie -> `String "cookie"
                
type parameter_style = Simple | Form [@@deriving show];;
let parameter_style_of_yojson = function
  | `String "simple" -> Simple
  | `String "form"   -> Form
  | x -> let open Ppx_yojson_conv_lib.Yojson_conv in
    raise (Of_yojson_error (Failure (sprintf "%s: unexpected value must be \"simple\" or \"form\"" __LOC__), x));;
;;

let yojson_of_parameter_style = function
  | Simple -> `String "simple"
  | Form   -> `String "form"

type parameter_object = {
  name              : string;
  _in               : parameter_location [@key "in"];
  description       : string option
                      [@default None] [@yojson_drop_default (=)];
  required          : bool option
                      [@default None] [@yojson_drop_default (=)];
  deprecated        : bool option
                      [@default None] [@yojson_drop_default (=)];
  allow_empty_value : bool option [@key "allowEmptyValue"]
                      [@default None] [@yojson_drop_default (=)];
  style             : parameter_style option [@default None] [@yojson_drop_default (=)];
  schema            : schema or_ref option [@default None] [@yojson_drop_default (=)];
  example           : any option [@default None] [@yojson_drop_default (=)]
    
} [@@deriving make,show,yojson];;

type example_object = {
    summary       : string option
                    [@default None] [@yojson_drop_default (=)];
    description   : string option
                    [@default None] [@yojson_drop_default (=)];
    value         : any option
                    [@default None] [@yojson_drop_default (=)];
    externalValue : string option
                    [@default None] [@yojson_drop_default (=)];
} [@@deriving make, show, yojson];;

type media_type_object = {
    schema   : schema or_ref option;
    example  : any option;
    examples : example_object or_ref map;
    encoding : any map option (* fixme: should be an encoding_object map *)
} [@@deriving make, show, yojson];;

type request_body_object = {
  description : string option [@default None] [@yojson_drop_default (=)];
  content     : any map;
  required    : bool option [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;

type security_scheme_object = {
    (* Note: these aren't actually all optional, which one is required
       depends on the type field. Should probably use a union type
       with custom json converters *)
  _type              : string [@key "type"];
  description        : string option
                       [@default None] [@yojson_drop_default (=)];
  name               : string option
                       [@default None] [@yojson_drop_default (=)];
  _in                : string option
                       [@key"in"] [@default None] [@yojson_drop_default (=)];
  scheme             : string option
                       [@default None] [@yojson_drop_default (=)];
  bearer_format      : string option
                       [@key "bearerFormat"] [@default None] [@yojson_drop_default (=)];
  (* FIXME: actually define Oauth Flows Object Type *)
  flows              : any option
                       [@default None] [@yojson_drop_default (=)];
  openid_connect_url : string option
                       [@key "openIdConnectUrl"] [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;

(* FIXME: define an actual type for this *)
type callback_object = any [@@deriving show,yojson];;

type components_object = {
    schemas          : schema or_ref map option
                       [@default None] [@yojson_drop_default (=)];
    responses        : response_object or_ref map option
                       [@default None] [@yojson_drop_default (=)];
    parameters       : parameter_object or_ref map option
                       [@default None] [@yojson_drop_default (=)];
    examples         : example_object or_ref map option
                       [@default None] [@yojson_drop_default (=)];
    request_bodies   : request_body_object or_ref map option
                       [@key "requestBodies"] [@default None] [@yojson_drop_default (=)];
    headers          : header_object or_ref map option
                       [@default None] [@yojson_drop_default (=)];
    security_schemes : security_scheme_object or_ref map option
                       [@key "securitySchemes"] [@default None] [@yojson_drop_default (=)];
    links            : link_object or_ref map option
                       [@default None] [@yojson_drop_default (=)];
    callbacks        : callback_object or_ref map option
                       [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type info_object = {
  title            : string;
  description      : string option [@default None] [@yojson_drop_default (=)];
  terms_of_service : string option [@key "termsOfService"] [@default None] [@yojson_drop_default (=)];
  contact          : contact_object option [@default None] [@yojson_drop_default (=)];
  license          : license_object option [@default None] [@yojson_drop_default (=)];
  version          : string;
} [@@deriving make,show,yojson];;

type operation_object = {
    tags          : string list option
                    [@default None] [@yojson_drop_default (=)];
    summary       : string option
                    [@default None] [@yojson_drop_default (=)];
    description   : string option
                    [@default None] [@yojson_drop_default (=)];
    external_docs : external_documentation_object option
                    [@key "externalDocs"] [@default None] [@yojson_drop_default (=)];
    operation_id  : string option
                    [@key "operationId"] [@default None] [@yojson_drop_default (=)];
    parameters    : parameter_object or_ref list option
                    [@default None] [@yojson_drop_default (=)];
    request_body  : request_body_object or_ref option
                    [@key "requestBody"] [@default None] [@yojson_drop_default (=)];
    responses     : responses_object;
    callbacks     : callback_object or_ref map option
                    [@default None] [@yojson_drop_default (=)];
    deprecated    : bool option
                    [@default None] [@yojson_drop_default (=)];
    (* FIXME: add a type for security_requirement *)
    security      : any option
                    [@default None] [@yojson_drop_default (=)];
    servers       : server_object list option
                    [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type path_object = {
  summary     : string option [@default None] [@yojson_drop_default (=)];
  description : string option [@default None] [@yojson_drop_default (=)];
  get         : operation_object option [@default None] [@yojson_drop_default (=)];
  put         : operation_object option [@default None] [@yojson_drop_default (=)];
  post        : operation_object option [@default None] [@yojson_drop_default (=)];
  delete      : operation_object option [@default None] [@yojson_drop_default (=)];
  options     : operation_object option [@default None] [@yojson_drop_default (=)];
  head        : operation_object option [@default None] [@yojson_drop_default (=)];
  patch       : operation_object option [@default None] [@yojson_drop_default (=)];
  trace       : operation_object option [@default None] [@yojson_drop_default (=)];
  servers     : server_object list option [@default None] [@yojson_drop_default (=)];
  parameters  : parameter_object or_ref list option [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;

type paths_object = path_object map [@@deriving yojson,show];;

type tag_object = {
  name : string;
  description : string option [@default None] [@yojson_drop_default (=)];
  external_docs : external_documentation_object option [@key "externalDocs"] [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type t = {
  openapi       : string;
  info          : info_object;
  servers       : server_object list option [@default None] [@yojson_drop_default (=)];
  paths         : paths_object;
  components    : components_object option [@default None] [@yojson_drop_default (=)];
  security      : any option [@default None] [@yojson_drop_default (=)]; (* FIXME: define an actual type 
                                                                            for security_requirements_object *)
  tags          : tag_object list option [@default None] [@yojson_drop_default (=)];
  external_docs : external_documentation_object option [@key "externalDocs"] [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;
