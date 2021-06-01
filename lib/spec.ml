open Core;;
open Base.Poly;;

type 'a map = (string * 'a) list [@@deriving show];;

let yojson_of_map ~f xs : Yojson.Safe.t =
  `Assoc (List.map ~f:(fun (k,v) -> (k, f v)) xs);;

let map_of_yojson ~(f:Yojson.Safe.t -> 'a) : Yojson.Safe.t ->'a map = function  
  | `Assoc xs -> List.map ~f:(fun (k,v) -> (k, (f v))) xs
  | x -> raise (Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure "unexpected input", x))

type any = Yojson.Safe.t;;
let pp_any = Yojson.Safe.pp;;
let any_of_yojson x = x;;
let yojson_of_any x = x;;

type any_map = any map [@@deriving show];;
let any_map_of_yojson = map_of_yojson ~f:(fun v -> v);; (* schema_object_of_yojson *);;
let yojson_of_any_map = yojson_of_map ~f:(fun v -> v);; (* yojson_of_schema_object *);;


type contact_object = {
  name  : string option [@default None] [@yojson_drop_default (=)];
  url   : string option [@default None] [@yojson_drop_default (=)];
  email : string option [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type license_object = {
  name : string;
  url : string option [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;


type server_variable_object = {
  enum : string list option [@default None] [@yojson_drop_default (=)];
  default : string;
  description : string option [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type external_documentation_object = {
  description : string option [@default None] [@yojson_drop_default (=)];
  url : string;
} [@@deriving make,show,yojson];;
  
type server_variable_map = server_variable_object map [@@deriving show];;
let yojson_of_server_variable_map = yojson_of_map ~f:yojson_of_server_variable_object;;
let server_variable_map_of_yojson = map_of_yojson ~f:server_variable_object_of_yojson;;


type server_object = {
  url : string;
  description : string option [@default None] [@yojson_drop_default (=)];
  variables : (string * server_variable_object) list option [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;


type schema_object = any;; (* Fixme: actually define a type for schema objects *)
let pp_schema_object = Yojson.Safe.pp;;
type schema_map = schema_object map [@@deriving show];;
let schema_map_of_yojson = map_of_yojson ~f:(fun v -> v);; (* schema_object_of_yojson *);;
let yojson_of_schema_map = yojson_of_map ~f:(fun v -> v);; (* yojson_of_schema_object *);;

type link_object = {
  operation_ref : string option [@key "operationRef"] [@default None] [@yojson_drop_default (=)];
  operation_id : string option [@key "operationId"] [@default None] [@yojson_drop_default (=)];
  parameters : any_map option [@default None] [@yojson_drop_default (=)];
  request_body : any  option [@key "requestBody"] [@default None] [@yojson_drop_default (=)]; (* this is actually defined as Any in the spec *)
  description : string option [@default None] [@yojson_drop_default (=)];
  server : server_object option [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type link_map = link_object map [@@deriving show];;
let link_map_of_yojson = map_of_yojson ~f:link_object_of_yojson;;
let yojson_of_link_map = yojson_of_map ~f:yojson_of_link_object;;

type header_object = {
  name: string;
  description : string option [@default None] [@yojson_drop_default (=)];
  external_docs : external_documentation_object option [@key "externalDocs"] [@default None] [@yojson_drop_default (=)];
}[@@deriving make,show,yojson];;
                                
type header_map = header_object map [@@deriving show];;
let header_map_of_yojson = map_of_yojson ~f:header_object_of_yojson;;
let yojson_of_header_map = yojson_of_map ~f:yojson_of_header_object;;

type response_object = {
  description : string;
  headers: header_map option [@default None] [@yojson_drop_default (=)];
  link : link_map option [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type response_map = response_object map [@@deriving show];;
let response_map_of_yojson = map_of_yojson ~f:response_object_of_yojson;;
let yojson_of_response_map = yojson_of_map ~f:yojson_of_response_object;;

type parameter_object = {
  name : string;
  _in  : string [@key "in"];
  description : string option [@default None] [@yojson_drop_default (=)];
  required : bool option [@default None] [@yojson_drop_default (=)];
  deprecated : bool option [@default None] [@yojson_drop_default (=)];
  allow_empty_value : bool option [@key "allowEmptyValue"] [@default None] [@yojson_drop_default (=)];
} [@@deriving make,show,yojson];;

type parameters_map = parameter_object map [@@deriving show];;
let parameters_map_of_yojson = map_of_yojson ~f:parameter_object_of_yojson;;
let yojson_of_parameters_map = yojson_of_map ~f:yojson_of_parameter_object;;

type example_object = {
  summary : string option [@default None] [@yojson_drop_default (=)];
  description : string option [@default None] [@yojson_drop_default (=)];
  value : any option [@default None] [@yojson_drop_default (=)]; (* this is actually defined as any in the spec *)
  externalValue : string option [@default None] [@yojson_drop_default (=)];
} [@@deriving make, show, yojson];;

type example_map = example_object map [@@deriving show];;
let example_map_of_yojson = map_of_yojson ~f:example_object_of_yojson;;
let yojson_of_example_map = yojson_of_map ~f:yojson_of_example_object;;

type request_body_object = {
  description : string option [@default None] [@yojson_drop_default (=)];
  content : any_map;
  required : bool option [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;

type request_map = request_body_object map [@@deriving show];;
let request_map_of_yojson = map_of_yojson ~f:request_body_object_of_yojson;;
let yojson_of_request_map = yojson_of_map ~f:yojson_of_request_body_object;;

type security_scheme_object = {
  (* Note: these aren't actually all optional, which one is required
     depends on the type field *)
  _type : string [@key "type"];
  description : string option [@default None] [@yojson_drop_default (=)];
  name : string option [@default None] [@yojson_drop_default (=)];
  _in : string option [@key"in"] [@default None] [@yojson_drop_default (=)];
  scheme : string option [@default None] [@yojson_drop_default (=)];
  bearer_format : string option [@key "bearerFormat"] [@default None] [@yojson_drop_default (=)];
  flows : any option [@default None] [@yojson_drop_default (=)]; (* FIXME: actually define Oauth Flows Object Type *)
  openid_connect_url : string option [@key "openIdConnectUrl"] [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;

type security_scheme_map = security_scheme_object map [@@deriving show];;
let security_scheme_map_of_yojson = map_of_yojson ~f:security_scheme_object_of_yojson;;
let yojson_of_security_scheme_map = yojson_of_map ~f:yojson_of_security_scheme_object;;


type callback_object = any;; (* FIXME: define an actual type for this *)
let pp_callback_object = Yojson.Safe.pp;;
type callback_map = callback_object map [@@deriving show];;
let callback_map_of_yojson = map_of_yojson ~f:(fun v -> v);;
let yojson_of_callback_map = yojson_of_map ~f:(fun v -> v);;

type components_object = {
  schemas          : schema_map option [@default None] [@yojson_drop_default (=)];
  responses        : response_map option [@default None] [@yojson_drop_default (=)];
  parameters       : parameters_map option [@default None] [@yojson_drop_default (=)];
  examples         : example_map option [@default None] [@yojson_drop_default (=)];
  request_bodies   : request_map option [@key "requestBodies"] [@default None] [@yojson_drop_default (=)];
  headers          : header_map option [@default None] [@yojson_drop_default (=)];
  security_schemes : security_scheme_map option [@key "securitySchemes"] [@default None] [@yojson_drop_default (=)];
  links            : link_map option [@default None] [@yojson_drop_default (=)];
  callbacks        : callback_map option [@default None] [@yojson_drop_default (=)];
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
  tags : string list option [@default None] [@yojson_drop_default (=)];
  summary : string option [@default None] [@yojson_drop_default (=)];
  description : string option [@default None] [@yojson_drop_default (=)];
  external_docs : external_documentation_object option [@key "externalDocs"] [@default None] [@yojson_drop_default (=)];
  operation_id : string option [@key "operationId"] [@default None] [@yojson_drop_default (=)];
  parameters : parameter_object list option [@default None] [@yojson_drop_default (=)];
  request_body : request_body_object option [@key "requestBody"] [@default None] [@yojson_drop_default (=)];
  responses : response_map;
  callbacks : callback_map option [@default None] [@yojson_drop_default (=)];
  deprecated : bool option [@default None] [@yojson_drop_default (=)];
  security : any option [@default None] [@yojson_drop_default (=)]; (* FIXME: add a schema for security_requirement *)
  servers : server_object list option [@default None] [@yojson_drop_default (=)];
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
  parameters  : parameter_object list option [@default None] [@yojson_drop_default (=)]
} [@@deriving make,show,yojson];;

type paths_object = path_object map [@@deriving show];;
let yojson_of_paths_object = yojson_of_map ~f:yojson_of_path_object;;
let paths_object_of_yojson = map_of_yojson ~f:path_object_of_yojson;;

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
