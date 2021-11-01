type t;;
val to_handler : t -> Rock.Handler.t
val empty : t;;

type builder = t -> t;;

val title            : string -> builder;;
val description      : string -> builder;;
val terms_of_service : string -> builder;;
val contact          : Spec.contact_object -> builder;;
val license          : Spec.license_object -> builder;;
val version          : string -> builder;;

type 'a or_ref = 'a Json_schema.or_ref;;

val schema           : string -> Json_schema.schema or_ref -> builder;;
val response         : string -> Spec.response_object or_ref -> builder;;
val parameter        : string -> Spec.parameter_object or_ref -> builder;;
val example          : string -> Spec.example_object or_ref -> builder;;
val request_body     : string -> Spec.request_body_object or_ref -> builder;;
val header           : string -> Spec.header_object or_ref -> builder;;
val security_scheme  : string -> Spec.security_scheme_object or_ref -> builder;;
val link             : string -> Spec.link_object or_ref -> builder;;
val callback         : string -> Spec.callback_object or_ref -> builder;;

val host             : string -> builder;;
val backlog          : int -> builder;;
val port             : int -> builder;;
val jobs             : int -> builder;;
val cmd_name         : string -> builder;;
val not_found        : (Opium.Request.t ->  (Httpaf.Headers.t * Rock.Body.t) Lwt.t) -> builder;;

type route = string -> Rock.Handler.t -> builder;;

type api_route = ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Json_schema.any Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    route;;

val get : api_route;;
val post : api_route;;
val delete : api_route;;
val put : api_route;;
val options : api_route;;
val head : api_route;;
val patch : api_route;;

val any : Opium.Method.t list -> route;;
val all : route;;
val action : Opium.Method.t -> route;;
val middleware : Rock.Middleware.t -> builder;;

val start : t -> Lwt_io.server Lwt.t;;
val start_multicore : t -> Lwt_io.server Lwt.t;;
val run_command  : t -> unit;;
val run_command' : t -> [> `Ok of unit Lwt.t | `Error | `Not_running ];;
val run_multicore  : t -> unit;;

