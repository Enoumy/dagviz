open Bonsai_web
open! Bonsai.Let_syntax
open! Core

module Styles =
[%css.raw
{|
 @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100&display=swap');
* {
  font-family: 'Roboto', sans-serif;
}

.card {
  box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
  transition: 0.3s;
  border-radius: 5px
}

|}]

let id_count = ref 0

module Id : sig
  type t

  val create : unit -> t
  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = string

  let to_string = Fn.id
  let of_string = Fn.id

  let create () =
    id_count := !id_count + 1;

    Int.to_string !id_count
end

module Node = struct
  type t =
    | Atom of { id : Id.t; chilldren : string list }
    | Group of { note : string; contents : t list }
end

let component =
  let%sub state, _set_state =
    Bonsai.state [%here] (module Int) ~default_model:1
  in
  let%arr _state = state in
  Vdom.Node.div [ Vdom.Node.text "End of transmission. Don't panic!" ]

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    component
