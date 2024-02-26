open Dream_html
open HTML

module type DB = Caqti_lwt.CONNECTION

module T = Caqti_type

let list_comments =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->* T.(tup2 int string)) "SELECT id, text FROM comment"
  in
  fun (module Db : DB) ->
    let%lwt comments_or_error = Db.collect_list query () in
    Caqti_lwt.or_fail comments_or_error

let add_comment =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit) "INSERT INTO comment (text) VALUES ($1)"
  in
  fun text (module Db : DB) ->
    let%lwt unit_or_error = Db.exec query text in
    Caqti_lwt.or_fail unit_or_error

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
          script
            [
              src "https://unpkg.com/htmx.org@1.9.10";
              integrity
                "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC";
              crossorigin `anonymous;
            ]
            "";
          body [ class_ "h-full" ] [ stuff ];
        ];
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

let render comments request =
  base_template
  @@ div []
       [
         ul []
           (comments
           |> List.map (fun (_id, comment) -> p [] [ txt "%s" comment ]));
         p []
           [
             form
               [ method_ `POST ]
               [ csrf_tag request; input [ name "text"; autofocus ] ];
           ];
       ]

let _unused =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  (* @@ Dream.sql_pool "postgresql://dream:password@postgres/dream" *)
  (* @@ Dream.sql_sessions *)
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream_html.respond @@ base_template Index.page);
         Dream.get "/login" (fun req ->
             Dream_html.respond @@ base_template @@ Auth.login_page req);
         Dream.get "/courses" (fun _req ->
             Dream_html.respond @@ base_template
             @@ Schedule.show_in_month (CalendarLib.Date.today ()));
         Dream.get "/calendar" (fun _ ->
             Dream_html.respond @@ base_template
             @@ Schedule.show_in_month (CalendarLib.Date.today ()));
         ( Dream.get "calendar/:year/:month/:day" @@ fun req ->
           let year = int_of_string @@ Dream.param req "year" in
           let month = int_of_string @@ Dream.param req "month" in
           let day = int_of_string @@ Dream.param req "day" in
           let date = CalendarLib.Date.make year month day in
           Dream_html.respond @@ Schedule.show_in_month date );
         Dream.post "/login" login_request_handler;
         Dream.get "/signup" (fun req ->
             Dream_html.respond @@ base_template @@ Auth.signup_page req);
         Dream.post "/signup" login_request_handler;
         Dream.post "/publish" login_request_handler;
         Dream.get "/db" (fun request ->
             let%lwt comments = Dream.sql request list_comments in
             Dream_html.respond (render comments request));
         Dream.post "/db" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("text", text) ] ->
                 let%lwt () = Dream.sql request (add_comment text) in
                 Dream.redirect request "/"
             | _ -> Dream.empty `Bad_Request);
       ]
