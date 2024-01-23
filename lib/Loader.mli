open Core
open Forester
open Forest

module Loader : sig
  val forest : string -> forest
  val parse : string -> Syn.t
end

val load : Eio_unix.Stdenv.base -> (unit -> 'a) -> 'a
