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
  module T : sig
    type t [@@deriving compare, sexp]
  end

  include T
  include module type of Comparable.Make (T)

  val create : unit -> t
  val to_string : t -> string
  val of_string : string -> t
end = struct
  module T = String
  include T
  include Comparable.Make (T)

  let to_string = Fn.id
  let of_string = Fn.id

  let create () =
    id_count := !id_count + 1;

    Int.to_string !id_count
end

module Node = struct
  type t =
    | Atom of { id : Id.t; children : Id.t list }
    | Group of { id : Id.t; contents : t list }

  let id = function Atom { id; _ } -> id | Group { id; _ } -> id
  let atom children = Atom { id = Id.create (); children }

  let rec depends_on (t : t) : Id.Set.t =
    let _t = t in
    Id.Set.empty
end

module Dag = struct
  type t = Node.t list

  let topological_sort (t : t) P Node.t list list
end

let _example =
  let open Node in
  let source1 = atom [] in
  let source2 = atom [] in
  let transform1 = atom [ id source1; id source2 ] in
  let transform2 = atom [ id transform1 ] in
  [ source1; source2; transform1; transform2 ]

let component =
  let%sub state, _set_state =
    Bonsai.state [%here] (module Int) ~default_model:1
  in
  let%arr _state = state in
  Vdom.Node.div [ Vdom.Node.text "End of transmission. Don't panic!" ]

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    component
