open Effect
open Forester
open Core
open Effect.Deep
open Dream_html
open HTML
open Scanner
(* open Scanner *)

type _ Effect.t +=
  | Page : string -> Dream_html.node t
  | Tooltip : string -> Dream_html.node t
  | Query : string -> Dream_html.node t
  | Toc : string -> Dream_html.node t
  | Parse : string -> Code.node Range.located list t
  | Forest : string -> Forest.forest t
(* | New_tree : prefix -> unit t *)

let eval_query q =
  let parse_query q = q in
  parse_query q |> fun a -> perform (Query a)

let page addr = perform (Page addr)
let tooltip addr = perform (Tooltip addr)
let toc addr = perform (Toc addr)
let load_forest dirs = perform (Forest dirs)
let make_dir ~env dir = Eio.Path.(Eio.Stdenv.fs env / dir)
let make_dirs ~env = List.map (make_dir ~env)

let render _forest f =
  try_with f ()
  @@ {
       effc =
         (fun (type a) (eff : a t) ->
           match eff with
           | Page addr ->
               Some
                 (fun (k : (a, _) continuation) ->
                   (* let doc = M.find_opt addr forest in *)
                   continue k
                     (div [] [ txt "%s" "Effectfully, "; txt "%s" addr ]))
           | Tooltip addr ->
               Some
                 (fun (k : (a, _) continuation) ->
                   continue k (div [] [ txt "%s" "Tooltip: "; txt "%s" addr ]))
           | Query q ->
               Some
                 (fun (k : (a, _) continuation) ->
                   continue k (div [] [ txt "%s" "Query: "; txt "%s" q ]))
           | _ -> None);
     }

let load env f =
  try_with f ()
  @@ {
       effc =
         (fun (type a) (eff : a t) ->
           match eff with
           (* | Parse filepath -> *)
           (*     Some *)
           (*       (fun (k : (a, _) continuation) -> *)
           (*         continue k (parse_file filepath)) *)
           | Forest dirs ->
               Some
                 (fun (k : (a, _) continuation) ->
                   continue k
                     (Forest.plant_forest
                     @@ read_trees_in_dirs ~dev:true
                     @@ make_dirs ~env [ dirs ]))
           | _ -> None);
     }
