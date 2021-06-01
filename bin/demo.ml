open Opium;;
module App = Openapi.App;;

App.empty ()
|> App.get "/foo/:foo" (fun req -> Response.of_json (`Assoc ["foo", `String (Router.param req "foo")])
                                   |> Lwt.return)
|> App.post
  "/bar/:foo" (fun req -> let open Lwt in
                          Request.to_plain_text req
                          >>= fun body -> Lwt_io.printf "body: \"%s\"\n" body
                          >|= fun _ -> 
                          Response.of_json (`Assoc ["foo", `String (Router.param req "foo");
                                                    "body", `String body]))
|> App.port 8888
|> App.cmd_name "demo"
|> App.run_command
  
    
