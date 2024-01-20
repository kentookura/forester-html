open Core
open Sem
open Dream_html
open HTML

let render_option r o = match o with Some o -> r o | None -> txt ""

let toc_item content =
  li []
    [
      a [ href ""; class_ "bullet"; title_ "%s" content ] [ txt "%s" "■" ];
      a [] [ span [ class_ "toc-item-label" ] [ txt "%s" content ] ];
    ]

(*  TODO: numbering logic *)
let toc doc = ul [ class_ "block" ] (List.map toc_item doc)

let backmatter (doc : tree) =
  let content = match doc.taxon with Some t -> [ txt "%s" t ] | None -> [] in
  section [ class_ "block" ] content

let mainmatter ?mode:_string _tree = div [] [ backmatter _tree ]
let render_title _ = div [] []
let addr _ = div [] []
let source_path _ = div [] []
let author _ = div [] []
let render_meta _ = div [] []
let date _ = li [] [ a [] [] ]

let _frontmatter (tree : tree) =
  header []
    [
      h1 []
        [
          span [ class_ "taxon" ] [ txt "taxon" ];
          render_title tree;
          addr tree;
          source_path tree;
        ];
      div
        [ class_ "metadata" ]
        [ ul [] [ date tree; author tree; render_meta tree ] ];
    ]

let render_tree (doc : tree) =
  (* REF: forest.xsl line 198 *)
  let open Dream_html in
  let open HTML in
  let render_node _ = div [] [] in
  let render_taxon doc = div [] [ render_option (txt "%s") doc.taxon ] in
  let render_source_path doc =
    div [] [ render_option (txt "%s") doc.source_path ]
  in
  let render_date _doc = div [] [] in
  let render_authors _doc = div [] [] in
  let render_title _doc = div [] [] in

  let render_meta (_key, body) = div [] [ render_node body ] in
  let render_metas doc = div [] (List.map render_meta doc.metas) in
  li []
    [
      render_taxon doc;
      render_source_path doc;
      render_date doc;
      render_authors doc;
      render_title doc.title;
      render_metas doc;
    ]
(* li [] [ List.map  ] *)

let page doc =
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
          link [ rel "stylesheet"; href "static/style.css" ];
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
                    [ a [ href "index.html" ] [ txt "« Home" ] ];
                ];
            ];
          div
            [ id "grid-wrapper" ]
            [
              article [] [ render_tree doc ];
              nav
                [ id "toc" ]
                [
                  div
                    [ class_ "block" ]
                    [ h1 [] [ txt "Table of contents" ]; toc [] ];
                ];
            ];
        ];
    ]
