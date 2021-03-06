(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
                      Thomas Refis  <refis.thomas(_)gmail.com>
                      Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

type extension = string * string list * string list

let ext_lwt : extension = "lwt",
  ["module Lwt : sig
      val un_lwt : 'a Lwt.t -> 'a
      val in_lwt : 'a Lwt.t -> 'a Lwt.t
      val to_lwt : 'a -> 'a Lwt.t
      val finally' : 'a Lwt.t -> unit Lwt.t -> 'a Lwt.t
      val un_stream : 'a Lwt_stream.t -> 'a
      val unit_lwt : unit Lwt.t -> unit Lwt.t
    end"],
  ["val (>>) : unit Lwt.t -> 'a Lwt.t -> 'a Lwt.t
    val raise_lwt : exn -> 'a Lwt.t"]

let ext_any : extension = "any",
  ["module Any : sig
      val val' : 'a
    end"],
  []

let ext_js : extension = "js",
  ["module Js : sig
      val un_js : 'a Js.t -> 'a
      val un_meth : 'a Js.meth -> 'a
      val un_constr : 'a Js.constr -> 'a
      val un_prop : 'a Js.gen_prop -> 'a
    end"],
  []

let ext_ounit : extension = "ounit",
  ["module OUnit : sig
      val force_bool : bool -> unit
      val force_unit : unit -> unit
      val force_unit_arrow_unit : (unit -> unit) -> unit
      val force_indexed : (int -> unit -> unit) -> int list -> unit
    end"],
  []

let registry = [ext_lwt;ext_js;ext_any;ext_ounit]

