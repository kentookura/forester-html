open Forester
open Forest
open Dream_html

module type Handler = sig
  val page : string -> node
  val tooltip : string -> node
  val query : string -> node
  val toc : string -> node
  val author : string -> node
  val get_doc : string -> node
  val parents : string -> node
  val children : string -> node
  val backlinks : string -> node
  val transclusion : string -> node
end

module Loader : sig
  val load : Eio_unix.Stdenv.base -> (unit -> 'a) -> 'a
  val forest : string -> forest
end
