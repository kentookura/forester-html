(*  TODO: Sentence case*)
(*  TODO: numbering logic, can prolly do it with css *)

open Core
open Sem
open Dream_html
open HTML
open Render_effect

(* include Render_effect *)
module Render_effect = Render_effect
open Forester

(* module H : Handler = struct *)
(*   (* let parse _addr = div [] [] *) *)
(*   (* let load_forest _addr = div [] [] *) *)
(*   (* let route _addr = div [] [] *) *)
(* end *)

let _render_option r o = match o with Some o -> r o | None -> txt ""
let _render_transclusion _doc = txt ""

let rec render_verbatim (located : Sem.node Range.located) =
  match located.value with
  | Sem.Text t -> txt "%s" t
  | Sem.Math (_, xs) -> div [] @@ List.map render_verbatim xs
  | _node ->
      Reporter.fatalf ?loc:located.loc Type_error
        "Render_verbatim: cannot render this kind of object."

and render_node (tree : Sem.node) =
  match tree with
  | Sem.Text t -> txt "%s" t
  | Sem.Math (mode, body) ->
      let attrs = match mode with Inline -> [] | Display -> [ id "" ] in
      div attrs @@ List.map render_verbatim body
  (* | Sem.Link { title; dest; _ } -> ( *)
  (*     match E.get_doc dest with *)
  (*     | Some _ -> render_internal_link ~title ~addr:dest *)
  (*     | None -> render_external_link ~title ~addr:dest) *)
  (* | Sem.Transclude (_opts, addr) -> ( *)
  (*     match E.get_doc addr with *)
  (*     | None -> txt "" *)
  (*     | Some doc -> render_transclusion doc) *)
  | Sem.Query (_, _)
  | Sem.Xml_tag (_, _, _)
  | Sem.Unresolved _ | Sem.Embed_tex _ | Sem.Img _
  | Sem.Block (_, _)
  | Sem.If_tex (_, _)
  | Sem.Prim (_, _)
  | Sem.Object _ ->
      Dream_html.txt "%s" "todo"
  | _ -> Dream_html.txt "%s" "todo"

let toc_item content =
  li []
    [
      a [ href ""; class_ "bullet"; title_ "%s" content ] [ txt "%s" "■" ];
      a [] [ span [ class_ "toc-item-label" ] [ txt "%s" content ] ];
    ]

let toc doc = ul [ class_ "block" ] (List.map toc_item doc)

let backmatter (doc : tree) =
  let content = match doc.taxon with Some t -> [ txt "%s" t ] | None -> [] in
  section [ class_ "block" ] content

let mainmatter ?mode:_string _tree = div [] [ backmatter _tree ]
let addr _ = div [] []
let source_path _ = div [] []
let author _ = div [] []
let date _ = li [] [ a [] [] ]

let meta_item (_i, (t : Sem.node Range.located list)) =
  li [ class_ "meta-item" ]
  @@ (t |> List.map (fun (n : Sem.node Range.located) -> render_node n.value))

let render_date (date : Prelude.Date.t) =
  let open Prelude.Date in
  let m =
    match month date with
    | Some n -> (
        match n with
        | 1 -> "January"
        | 2 -> "February"
        | 3 -> "March"
        | 4 -> "April"
        | 5 -> "May"
        | 6 -> "June"
        | 7 -> "July"
        | 8 -> "August"
        | 9 -> "September"
        | 10 -> "October"
        | 11 -> "November"
        | 12 -> "December"
        | _ -> "")
    | None -> ""
  in
  let d = match day date with Some n -> Int.to_string n | None -> "" in
  let y = Int.to_string @@ year date in
  m ^ " " ^ d ^ ", " ^ y

let render_meta (tree : tree) =
  div
    [ class_ "metadata" ]
    [
      ul []
      @@ List.map
           (fun d -> li [ class_ "meta-item" ] [ txt "%s" (render_date d) ])
           tree.dates
      @ [
          li
            [ class_ "meta-item" ]
            [
              (*  TODO: get author name effectfully *)
              address [ class_ "author" ]
              @@ [ txt "%s" @@ (String.concat ", ") tree.authors ];
              (*  TODO: with contributions from ... *)
            ];
        ];
    ]

let frontmatter (tree : tree) =
  let taxon = match tree.taxon with None -> "" | Some t -> t in
  let title =
    match tree.title with
    | None -> div [] []
    | Some ts ->
        div []
        @@ List.map (fun (t : Sem.node Range.located) -> render_node t.value) ts
  in
  let slug =
    match tree.addr with
    | None -> div [] []
    | Some s -> a [ class_ "slug"; href "%s.html" s ] [ txt "[%s]" s ]
  in
  header []
    [
      h1 [] [ span [ class_ "taxon" ] [ txt "%s" taxon ]; title; slug ];
      render_meta tree;
    ]

let render_tree (doc : tree) =
  let id =
    match doc.addr with None -> id "unknown_tree" | Some addr -> id "%s" addr
  in

  article []
    [
      section
        [ class_ "block"; id ]
        [
          details [ open_ ]
            [ summary [] [ frontmatter doc ]; div [ class_ "tree-content" ] [] ];
        ];
      nav [ (*id "toc" *) ]
        [ div [ class_ "block" ] [ h1 [] [ txt "Table of contents" ]; toc [] ] ];
    ]

and fourohfour addr = txt "%s not found" addr
and with_fallback fof f got = match got with Some t -> f t | None -> fof

(* and index = *)
(*   let open Dream_html in *)
(*   let open HTML in *)
(*   Forest.complete ~forest "" |> List.of_seq *)
(*   |> List.map (fun (addr, title) -> div [] [ txt "%s" addr; txt "%s" title ]) *)

and base_template doc =
  html
    [ lang "en" ]
    [
      head []
        [
          meta [ name "viewport"; content "width=device-width" ];
          title [] "HTMX+Dream+Effects";
          script
            [
              src "https://unpkg.com/htmx.org@1.9.10";
              integrity
                "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC";
              crossorigin `anonymous;
            ]
            "";
          link [ rel "stylesheet"; href "/static/style.css" ];
          link
            [
              rel "stylesheet";
              href
                "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css";
              integrity
                "sha384-n8MVd4RsNIU0tAv4ct0nTaAbDJwPJzDEaqSD1odI+WdtXRGWt2kTvGFasHpSy3SV";
              crossorigin `anonymous;
            ];
          script
            [
              defer;
              src "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js";
              integrity
                "sha384-XjKyOOlGwcjNTAIQHIpgOno0Hl1YQqzUOEleOLALmuqehneUG+vnGctmUb0ZY0l8";
              crossorigin `anonymous;
            ]
            "";
          script
            [
              defer;
              src
                "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js";
              integrity
                "sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05";
              crossorigin `anonymous;
              string_attr "onload" "renderMathInElement(document.body);";
            ]
            "";
        ];
      body []
        [
          header
            [ class_ "header" ]
            [
              nav
                [ class_ "nav" ]
                [
                  div
                    [ class_ "logo" ]
                    [ a [ href "/index.html" ] [ txt "« Home" ] ];
                ];
            ];
          div [ id "grid-wrapper" ] [ doc ];
        ];
    ]

let render (forest : Forest.forest) =
  (module struct
    module A = Analysis
    module M = A.Map

    let page addr =
      match M.find_opt addr forest.trees with
      | Some t -> base_template @@ render_tree t
      | None -> fourohfour addr

    let tooltip addr = div [] [ txt "%s" addr ]
    let query addr = div [] [ txt "%s" addr ]
    let toc addr = div [] [ txt "%s" addr ]
    let author addr = div [] [ txt "%s" addr ]
    let get_doc addr = div [] [ txt "%s" addr ]
    let parents addr = div [] [ txt "%s" addr ]
    let children addr = div [] [ txt "%s" addr ]
    let backlinks addr = div [] [ txt "%s" addr ]
    let transclusion addr = div [] [ txt "%s" addr ]
  end : Handler)
