open Core;;
open Base.Poly;;

module O = Opium.App;;

type t = {
  spec : Spec.t;
  app  : O.t;
};;

let to_handler a = O.to_handler a.app
let empty ?(title = "Application") ?description ?terms_of_service ?contact ?license ?(version = "0.1") () =
  {spec = Spec.make ~openapi:"3.0.0" ~info:(Spec.make_info_object ~title ?description ?terms_of_service ?contact ?license ~version ()) ~paths:[] ();
   app = O.empty}

type builder = t -> t
let version v a = {a with spec = {a.spec with info = {a.spec.info with version = v}}};;
let host s a = {a with app = O.host s a.app};;
let backlog i a = {a with app = O.backlog i a.app};;
let port p a = {a with app = O.port p a.app};;
let jobs n a = {a with app = O.jobs n a.app};;
let cmd_name s a = {spec = {a.spec with info = {a.spec.info with title = s}};
                    app = O.cmd_name s a.app};;
let not_found f a = {a with app = O.not_found f a.app};;

type route = string -> Rock.Handler.t -> builder;;

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
                                   Spec.make_parameter_object ~name ~_in:"path" ~required:(Some true) ())

let merge_parameters (orig : Spec.parameter_object list) (add : Spec.parameter_object list) =
  List.fold_left ~init:orig add
    ~f:(fun orig -> fun p ->
        let open Spec in
        match List.find ~f:(fun op -> op.name = p.name) orig with
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

let post ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?request_body
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with post = Some (Spec.make_operation_object ?tags ?summary ?description
                                 ?external_docs ?operation_id
                                 ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                 ~request_body:(Option.value request_body
                                                  ~default:(Spec.make_request_body_object ~content:[("text/*", `Assoc[])] ())
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

let put ?tags ?summary ?description ?external_docs ?operation_id ?(parameters = []) ?request_body
    ?(responses = []) ?callbacks ?deprecated ?security ?servers path handler a =
  let p = List.Assoc.find ~equal:(=) a.spec.paths path
          |> Option.value ~default:(Spec.make_path_object ()) in
  let p = {p with put = Some (Spec.make_operation_object ?tags ?summary ?description
                                ?external_docs ?operation_id
                                ~parameters:(Some (merge_parameters parameters (extract_path_params path)))
                                ~request_body:(Option.value request_body
                                                  ~default:(Spec.make_request_body_object ~content:[("text/*", `Assoc[])] ())
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
