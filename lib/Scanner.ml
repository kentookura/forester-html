open Forester
open Core
open Lexing

module S = Algaeff.Sequencer.Make (struct
  type t = Eio.Fs.dir_ty Eio.Path.t
end)

let parse_channel filename ch =
  Reporter.tracef "when parsing file `%s`" filename @@ fun () ->
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Grammar.main Lexer.token lexbuf with
  | Grammar.Error ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "failed to parse `%s`"
        (Lexing.lexeme lexbuf)
  | Lexer.SyntaxError token ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "unrecognized token `%s`"
      @@ String.escaped token

let parse_file fp =
  let filename = Eio.Path.native_exn fp in
  let ch = open_in filename in
  Fun.protect ~finally:(fun _ -> close_in ch) @@ fun _ ->
  parse_channel filename ch

let read_tree fp =
  let source_path = None in
  let addr = "hello-addr" in
  match parse_file fp with
  | code -> Some Code.{ source_path; addr; code }
  | exception _exn -> None

let rec process_file fp =
  if Eio.Path.is_directory fp then process_dir fp
  else
    Eio.Path.split fp
    |> Option.iter @@ fun (_dir, basename) ->
       if
         Filename.extension basename = ".tree"
         && (not @@ String.starts_with ~prefix:"." basename)
       then S.yield fp

and process_dir fp =
  Eio.Path.with_open_dir fp @@ fun dir ->
  Eio.Path.read_dir dir
  |> List.iter @@ fun fp -> process_file Eio.Path.(dir / fp)

let scan_directories dirs = S.run @@ fun () -> dirs |> List.iter process_dir

let read_trees_in_dirs ~dev ?(ignore_malformed = false) dirs =
  scan_directories dirs
  |> Seq.filter_map @@ fun fp ->
     Option.bind (Eio.Path.split fp) @@ fun (_dir, basename) ->
     let addr = Filename.chop_extension basename in
     let source_path =
       if dev then Option.map Unix.realpath @@ Eio.Path.native fp else None
     in
     match parse_file fp with
     | code -> Some Code.{ source_path; addr; code }
     | exception exn -> if ignore_malformed then None else raise exn

let read_addrs_in_dirs dirs =
  scan_directories dirs
  |> Seq.filter_map @@ fun fp ->
     Option.bind (Eio.Path.split fp) @@ fun (_dir, basename) ->
     Option.some @@ Filename.chop_extension basename
