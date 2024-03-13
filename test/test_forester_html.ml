(* open Render_html.Loader *)
open Core
open Lexing
open Forester
open Dream_html
open HTML
open Render_html
module Path = Eio.Path

let ninja_keys = std_tag "ninja-keys"

let base_template stuff =
  html
    [ string_attr "xmlns:mml" "http://ww.w3.org/1998/Math/MathML" ]
    [
      head []
        [
          (* meta [ charset "UTF-8" ]; *)
          (* meta *)
          (*   [ name "viewport"; content "width=device-width, initial-scale=1.0" ]; *)
          meta [ http_equiv `content_type; content "text/html; charsetutf-8" ];
          meta [ name "viewport"; content "widthdevice-width" ];
          link [ rel "stylesheet"; href "style.css" ];
          link [ rel "stylesheet"; href "katex.min.css" ];
          body []
            [
              ninja_keys [ placeholder "Start typing a note title or ID" ] [];
              stuff;
            ];
        ];
    ]

let load_file filepath =
  Reporter.tracef "when parsing file `%s`" filepath @@ fun () ->
  let lexbuf = Lexing.from_channel (open_in filepath) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
  try Grammar.main Lexer.token lexbuf with
  | Grammar.Error ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "failed to parse `%s`"
        (Lexing.lexeme lexbuf)
  | Lexer.SyntaxError token ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "unrecognized token `%s`"
      @@ String.escaped token

module Tty = Asai.Tty.Make (Core.Reporter.Message)

let () =
  Eio_main.run @@ fun env ->
  let fatal diagnostics = Tty.display diagnostics in
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let module M = Forester.Analysis.Map in
  let code = load_file "deps/test.tree" in
  let tree : Core.Code.tree =
    { source_path = Some "test.tree"; addr = Some "test"; code }
  in
  let forest = [ tree ] |> List.to_seq |> Forest.plant_forest in
  let cfg =
    Forest.
      {
        env;
        root = None;
        base_url = None;
        assets_dirs = [];
        max_fibers = 20;
        ignore_tex_cache = true;
        no_assets = true;
        no_theme = true;
      }
  in
  forest.trees |> M.to_seq |> Seq.map snd
  |> Seq.iter (fun _tree ->
         print_endline @@ to_string @@ base_template
         @@ Page.render "test" forest);
  Forest.render_trees ~cfg ~forest
(* Unix.execv "saxon-he" [| "-s:output/test.xml"; "-xsl:forest.xsl" |] *)
