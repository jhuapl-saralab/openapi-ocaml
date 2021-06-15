open Core;;
open Base.Poly;;
module O = Opium.App;;

type t = {
  spec : Spec.t;
  app  : O.t;
};;

let to_handler a = O.to_handler a.app
let empty =
  {spec = Spec.make ~openapi:"3.0.0" ~info:(Spec.make_info_object ~title:"Application" ~version:"0.1" ()) ~paths:[] ();
   app = O.empty}

type builder = t -> t
let title t a = {a with spec = {a.spec with info = {a.spec.info with title = t}}}
let description d a =
    {a with spec = {a.spec with info = {a.spec.info with description = Some d}}}
let terms_of_service t a =
    {a with spec = {a.spec with info = {a.spec.info with terms_of_service = Some t}}}
let contact c a =
    {a with spec = {a.spec with info = {a.spec.info with contact = Some c}}}
let license l a =
    {a with spec = {a.spec with info = {a.spec.info with license = Some l}}}
let version v a = {a with spec = {a.spec with info = {a.spec.info with version = v}}};;

type 'a or_ref = 'a Json_schema.or_ref;;

let schema n s a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.schemas
                  |> fun ss -> {cs with schemas = Some ((n,s)::ss)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let response n r a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.responses
                  |> fun rs -> {cs with responses = Some ((n,r)::rs)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let parameter n p a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.parameters
                  |> fun ps -> {cs with parameters = Some ((n,p)::ps)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let example n ex a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.examples
                  |> fun ss -> {cs with examples = Some ((n,ex)::ss)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let request_body n r a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.request_bodies
                  |> fun ss -> {cs with request_bodies = Some ((n,r)::ss)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let header n h a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.headers
                  |> fun ss -> {cs with headers = Some ((n,h)::ss)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let security_scheme n s a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.security_schemes
                  |> fun ss -> {cs with security_schemes = Some ((n,s)::ss)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let link n l a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.links
                  |> fun ss -> {cs with links = Some ((n,l)::ss)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let callback n c a =
    let open Spec in
    Option.value ~default:(make_components_object ()) a.spec.components
    |> (fun cs -> Option.value ~default:[] cs.callbacks
                  |> fun ss -> {cs with callbacks = Some ((n,c)::ss)})
    |> Option.return
    |> fun cs -> {a with spec = {a.spec with components = cs}}

let host s a = {a with app = O.host s a.app};;
let backlog i a = {a with app = O.backlog i a.app};;
let port p a = {a with app = O.port p a.app};;
let jobs n a = {a with app = O.jobs n a.app};;
let cmd_name s a = {spec = {a.spec with info = {a.spec.info with title = s}};
                    app = O.cmd_name s a.app};;
let not_found f a = {a with app = O.not_found f a.app};;

type route = string -> Rock.Handler.t -> builder;;

type api_route = ?tags:string list option ->
    ?summary:string option ->
    ?description:string option ->
    ?external_docs:Spec.external_documentation_object option ->
    ?operation_id:string option ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref option ->
    ?responses:Spec.responses_object ->
    ?callbacks:Json_schema.any Json_schema.or_ref Json_schema.map option ->
    ?deprecated:bool option ->
    ?security:Json_schema.any option ->
    ?servers:Spec.server_object list option ->
    route;;

let rewrite_path p =
  String.split ~on:'/' p
  |> List.map ~f:(fun c -> match String.chop_prefix ~prefix:":" c with
      | Some c -> "{"^c^"}"
      | _      -> c)
  |> String.concat ~sep:"/"
    
let extract_path_params p = p
                            |> String.split ~on:'/'
                            |> List.filter_map
                              ~f:(fun s -> let open Option.Monad_infix in
                                   String.chop_prefix ~prefix:":" s
                                   >>| fun name ->
                                   Json_schema.Obj (Spec.make_parameter_object ~name ~_in:"path" ~required:(Some true) ()))

let merge_parameters orig add =
    let same_param
            (p1 :Spec.parameter_object Json_schema.or_ref)
            (p2 : Spec.parameter_object Json_schema.or_ref) =
        let open Json_schema in
        let open Spec in
        match (p1,p2) with
        | Ref r1, Ref r2 -> r1 = r2
        | Obj p1, Obj p2 -> p1.name = p2.name
        | _ -> false in
    List.fold_left ~init:orig add
        ~f:(fun orig -> fun p ->               
               match List.find ~f:(same_param p) orig with
               | Some _  -> orig
               | None    -> p::orig)
        
let get ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?request_body
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with get = Some (Spec.make_operation_object ?tags ?summary ?description
                                ?external_docs ?operation_id
                                ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                ?request_body ~responses ?callbacks ?deprecated ?security ?servers ())} in
  let paths = List.Assoc.add ~equal:(=) a.spec.paths (rewrite_path path) p in
  {spec = {a.spec with paths =  paths};
   app = O.get path handler a.app}

let post ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?(request_body = None)
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with post = Some (Spec.make_operation_object ?tags ?summary ?description
                                 ?external_docs ?operation_id
                                 ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                 ~request_body:(Option.value request_body
                                                  ~default:(Spec.make_request_body_object ~content:[("text/plain",
                                                                                                     `Assoc["schema", `Assoc []])] ()
                                                            |> fun o -> Json_schema.Obj o)
                                                |> Option.return)
                                 ~responses ?callbacks ?deprecated ?security ?servers ())} in
  let paths = List.Assoc.add ~equal:(=) a.spec.paths (rewrite_path path) p in
  {spec = {a.spec with paths =  paths};
   app = O.post path handler a.app}

let delete ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?request_body
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with delete = Some (Spec.make_operation_object ?tags ?summary ?description
                                   ?external_docs ?operation_id
                                   ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                   ?request_body ~responses ?callbacks ?deprecated ?security ?servers ())} in
  let paths = List.Assoc.add ~equal:(=) a.spec.paths (rewrite_path path) p in
  {spec = {a.spec with paths =  paths};
   app = O.delete path handler a.app}

let put ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?(request_body = None)
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with put = Some (Spec.make_operation_object ?tags ?summary ?description
                                ?external_docs ?operation_id
                                ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                ~request_body:(Option.value request_body
                                                 ~default:(Spec.make_request_body_object ~content:["text/plain",
                                                                                                   `Assoc["schema", `Assoc []]
                                                                                                  ] ()
                                                           |> (fun o -> Json_schema.Obj o))
                                               |> Option.return)
                                ~responses ?callbacks ?deprecated ?security ?servers ())} in
  let paths = List.Assoc.add ~equal:(=) a.spec.paths (rewrite_path path) p in
  {spec = {a.spec with paths =  paths};
   app = O.put path handler a.app}

let options ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?request_body
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with options = Some (Spec.make_operation_object ?tags ?summary ?description
                                    ?external_docs ?operation_id
                                    ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                    ?request_body ~responses ?callbacks ?deprecated ?security ?servers ())} in
  let paths = List.Assoc.add ~equal:(=) a.spec.paths (rewrite_path path) p in
  {spec = {a.spec with paths = paths};
   app = O.options path handler a.app}

let head ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?request_body
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with head = Some (Spec.make_operation_object ?tags ?summary ?description
                                 ?external_docs ?operation_id
                                 ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                 ?request_body ~responses ?callbacks ?deprecated ?security ?servers ())} in
  let paths = List.Assoc.add ~equal:(=) a.spec.paths (rewrite_path path) p in
  {spec = {a.spec with paths =  paths};
   app = O.head path handler a.app}

let patch ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?request_body
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with patch = Some (Spec.make_operation_object ?tags ?summary ?description
                                  ?external_docs ?operation_id
                                  ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                  ?request_body ~responses ?callbacks ?deprecated ?security ?servers ())} in
  let paths = List.Assoc.add ~equal:(=) a.spec.paths (rewrite_path path) p in
  {spec = {a.spec with paths =  paths};
   app = O.patch path handler a.app}

let any ms p h a = {a with app = O.any ms p h a.app}
let all p h a = {a with app = O.all p h a.app}
let action m p h a = {a with app = O.action m p h a.app}
let middleware m a = {a with app = O.middleware m a.app}

let api app = let open Opium in
  {app with app = app.app
                  |> O.get "/openapi.json"
                    (fun _req -> Spec.yojson_of_t app.spec
                                 |> Response.of_json
                                 |> Lwt.return)
                  |> O.get "/docs"
                    (fun _req ->
                       Response.of_html
                         (let open Tyxml.Html in
                          html
                            (head (title (txt (sprintf "Swagger %s UI" app.spec.info.title)))
                               [link ~rel:[`Stylesheet] ~href:("https://cdn.jsdelivr.net/npm/swagger-ui-dist@3/swagger-ui.css")
                                  ~a:[a_mime_type "text/css"] ();
                                link ~rel:[`Other "shortcut icon"] ~href:"https://fastapi.tiangolo.com/img/favicon.png" ()])
                            (body [
                                div ~a:[a_id "swagger-ui"] [];
                                script ~a:[a_src "https://cdn.jsdelivr.net/npm/swagger-ui-dist@3/swagger-ui-bundle.js"] (cdata_script "");
                                script (cdata_script (
                                    "const ui = SwaggerUIBundle({
                         url: '/openapi.json',
                         oauth2RedirectUrl: window.location.origin + '/docs/oauth2-redirect',
                         dom_id: '#swagger-ui',
                         presets: [
                            SwaggerUIBundle.presets.apis,
                            SwaggerUIBundle.SwaggerUIStandalonePreset
                         ],
                         layout: 'BaseLayout',
                         deepLinking: true,
                         showExtensions: true,
                         showCommonExtensions: true
                     })"))]))
                       |> Lwt.return)}


let start a = O.start ((api a).app)
let start_multicore a = O.start ((api a).app)
let run_command a = O.run_command ((api a).app)
let run_command' a = O.run_command' ((api a).app)
let run_multicore a = O.run_multicore ((api a).app)
