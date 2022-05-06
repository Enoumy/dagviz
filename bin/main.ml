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

.vbox {
  display: flex;
  flex-direction: column;
  flex-wrap: nowrap;
  justify-content: space-evenly;
  padding : 8px;
  margin : 8px;
}

.hbox {
  display: flex;
  flex-direction: row;
  flex-wrap: nowrap;
  justify-content: space-evenly;
  margin : 8px;
  
}

|}]

module View = struct
  type t = Vdom.Node.t

  open Vdom.Node
  open Vdom.Attr

  let hbox children =
    div ~attr:(many [ class_ Styles.hbox; class_ Styles.card ]) children

  let vbox children =
    div ~attr:(many [ class_ Styles.vbox; class_ Styles.card ]) children

  let text s = Vdom.Node.text s
end

let id_count = ref 0

module Id : sig
  module T : sig
    type t [@@deriving compare, sexp, equal]
  end

  type t = T.t [@@deriving sexp]

  include T with type t := t
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
  [@@deriving sexp]

  let id = function Atom { id; _ } -> id | Group { id; _ } -> id
  let atom children = Atom { id = Id.create (); children }

  let rec dependencies (t : t) : Id.Set.t =
    match t with
    | Atom { children; _ } -> Id.Set.of_list children
    | Group { contents; _ } ->
        let content_ids = Id.Set.of_list (List.map contents ~f:id) in
        let references_by_content =
          List.fold contents ~init:Id.Set.empty ~f:(fun acc content ->
              Set.union acc (dependencies content))
        in

        Set.diff references_by_content content_ids
end

module Dag = struct
  type t = Node.t list [@@deriving sexp]

  let topological_sort (t : t) : Node.t list list =
    let am_i_dependency_free ~(me : Node.t) ~(others : Node.t list) : bool =
      List.exists others ~f:(fun peer ->
          let is_equal = Id.equal (Node.id me) (Node.id peer) in

          let depends_on_me =
            lazy
              (let peer_dependencies = Node.dependencies peer in
               Set.mem peer_dependencies (Node.id me))
          in
          (not is_equal) && not (Lazy.force depends_on_me))
    in

    let rec loop ~(acc : Node.t list list) (values_to_sort : Node.t list) =
      match values_to_sort with
      | [] -> List.rev acc
      | _ -> (
          let curr_top_level, remaining_values =
            List.partition_tf values_to_sort ~f:(fun value ->
                am_i_dependency_free ~me:value ~others:values_to_sort)
          in
          match curr_top_level with
          | [] -> failwith "cycle... i think?"
          | _ -> loop ~acc:(curr_top_level :: acc) remaining_values)
    in

    loop ~acc:[] t

  let rec to_view (t : t) : View.t =
    let node_to_view (node : Node.t) : View.t =
      match node with
      | Atom { id; _ } -> View.hbox [ View.text (Id.to_string id) ]
      | Group { contents; id } ->
          View.vbox [ View.text (Id.to_string id); to_view contents ]
    in

    let topological_sort = topological_sort t in
    View.hbox
      (List.map topological_sort ~f:(fun level ->
           View.vbox (List.map level ~f:node_to_view)))
end

let example : Dag.t =
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
  print_s [%message (example : Dag.t)];
  Dag.to_view example

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    component
