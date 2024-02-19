open Dream_html
open HTML

let redirect ?message _request =
  match message with
  | None -> null []
  | Some message -> p [] [ txt "%s" message ]

let base_template stuff =
  html
    [ class_ "h-full" ]
    [
      head []
        [
          meta [ charset "UTF-8" ];
          meta
            [ name "viewport"; content "width=device-width, initial-scale=1.0" ];
          script [ src "https://cdn.tailwindcss.com?plugins=forms" ] "";
        ];
      body [ class_ "h-full" ] [ stuff ];
    ]

let login_request_handler req =
  match%lwt Dream.form req with
  | `Ok [ ("message", message) ] -> Dream_html.respond (redirect ~message req)
  | `Many_tokens _ -> Dream_html.respond (txt "many tokens")
  | `Missing_token _ -> Dream_html.respond (txt "missing tokens")
  | `Invalid_token _ -> Dream_html.respond (txt "invalid token")
  | `Wrong_session _ -> Dream_html.respond (txt "wrong session")
  | `Ok _ -> Dream_html.respond (base_template @@ txt "ok")
  | `Expired _ -> Dream_html.respond (txt "expired")
  | `Wrong_content_type -> Dream_html.respond (txt "wront content type")

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.sql_pool "sqlite3:db.sqlite"
  @@ Dream.sql_sessions
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream_html.respond @@ base_template Index.page);
         Dream.get "/login" (fun req ->
             Dream_html.respond @@ base_template @@ Auth.login_page req);
         Dream.post "/login" login_request_handler;
         Dream.get "/signup" (fun req ->
             Dream_html.respond @@ base_template @@ Auth.signup_page req);
         Dream.post "/signup" login_request_handler;
         Dream.post "/publish" login_request_handler;
       ]
