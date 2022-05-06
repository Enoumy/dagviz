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

.group {
  border-style: dotted;
  cursor: pointer;
}

|}]

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

module View = struct
  type t = Vdom.Node.t

  open Vdom.Node
  open Vdom.Attr

  let hbox children =
    div ~attr:(many [ class_ Styles.hbox; class_ Styles.card ]) children

  let vbox children =
    div ~attr:(many [ class_ Styles.vbox; class_ Styles.card ]) children

  let text s = Vdom.Node.text s

  let group ~(toggle_collapsed : unit Effect.t) children =
    div
      ~attr:
        (many
           [
             class_ Styles.vbox;
             class_ Styles.group;
             on_click (Fn.const toggle_collapsed);
           ])
      children

  let none = Vdom.Node.none
end

module Node = struct
  type t =
    | Atom of { id : Id.t; children : Id.t list }
    | Group of { id : Id.t; contents : t list }
  [@@deriving sexp]

  let id = function Atom { id; _ } -> id | Group { id; _ } -> id
  let atom children = Atom { id = Id.create (); children }
  let group contents = Group { id = Id.create (); contents }

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
    let does_anything_depend_on_me ~(me : Node.t) ~(others : Node.t list) : bool
        =
      let ans =
        not
          (List.exists others ~f:(fun peer ->
               let depends_on_me =
                 let peer_dependencies = Node.dependencies peer in
                 Set.mem peer_dependencies (Node.id me)
               in
               depends_on_me))
      in
      print_s
        [%message
          "does_anything_depend_on_me"
            (me : Node.t)
            (others : Node.t list)
            (ans : bool)];
      ans
    in

    let rec loop ~(acc : Node.t list list) (values_to_sort : Node.t list) =
      match values_to_sort with
      | [] -> List.rev acc
      | _ -> (
          let curr_top_level, remaining_values =
            List.partition_tf values_to_sort ~f:(fun value ->
                does_anything_depend_on_me ~me:value ~others:values_to_sort)
          in
          match curr_top_level with
          | [] -> failwith "cycle... i think?"
          | _ -> loop ~acc:(curr_top_level :: acc) remaining_values)
    in

    loop ~acc:[] t

  let rec to_view ~(collapsed : Id.Set.t)
      ~(toggle_collapsed : Id.t -> unit Effect.t) (t : t) : View.t =
    let node_to_view (node : Node.t) : View.t =
      let deps = Node.dependencies node |> Id.Set.sexp_of_t |> Sexp.to_string in
      match node with
      | Atom { id; _ } ->
          View.hbox [ View.text [%string "%{(Id.to_string id)} %{deps}"] ]
      | Group { contents; id } ->
          let content =
            match Set.mem collapsed id with
            | true -> View.none
            | false -> to_view ~collapsed ~toggle_collapsed contents
          in

          View.group ~toggle_collapsed:(toggle_collapsed id)
            [
              View.text [%string "%{(Id.to_string id)} %{deps}"]; content;
            ]
    in

    let topological_sort = topological_sort t in
    print_s [%message (topological_sort : Node.t list list)];
    View.hbox
      (List.map topological_sort ~f:(fun level ->
           View.vbox (List.map level ~f:node_to_view)))
end

let example1 : Dag.t =
  let open Node in
  let source1 = atom [] in
  let source2 = atom [] in
  let transform1 = atom [ id source1; id source2 ] in
  let transform2 = atom [ id transform1 ] in
  [ source1; source2; transform1; transform2 ]

let example2 : Dag.t =
  let open Node in
  let source1 = atom [] in
  let source2 = atom [] in
  let source3 = atom [] in
  let transform1 = atom [ id source1; id source2; id source3 ] in
  let transform2 = atom [ id transform1 ] in
  let transform2' = atom [ id transform1 ] in
  let transform3 = atom [ id transform2; id source3 ] in
  let transform3' = atom [ id transform2; id source2 ] in
  [
    source1;
    source2;
    transform1;
    transform2;
    source3;
    transform3;
    transform2';
    transform3';
  ]

let groups =
  let open Node in
  let group1 = group example1 in
  let group2 = group example1 in
  let group3 = group example2 in
  let atom = atom [ id group1; id group2; id group3 ] in
  [ atom; group1; group2; group3 ]

let group_with_external_deps =
  let open Node in
  let source = atom [] in
  let transform = atom [ id source ] in
  let transform2 = atom [ id source; id transform ] in
  let group = group [ transform; transform2 ] in
  let top = atom [ id group ] in
  [ top; group; source ]

let examples : (string * Dag.t) list =
  [
    ("example1", example1);
    ("example2", example2);
    ("groups", groups);
    ("group with external deps", group_with_external_deps);
  ]

let component =
  let%sub collapsed, toggle_collapsed =
    Bonsai.state_machine0 [%here]
      (module Id.Set)
      (module Id)
      ~default_model:Id.Set.empty
      ~apply_action:(fun ~inject:_ ~schedule_event:_ collapsed toggle_id ->
        match Set.mem collapsed toggle_id with
        | false -> Set.add collapsed toggle_id
        | true -> Set.remove collapsed toggle_id)
  in
  let%arr collapsed = collapsed and toggle_collapsed = toggle_collapsed in
  Vdom.Node.div
    (List.map examples ~f:(fun (name, dag) ->
         Vdom.Node.div
           ~attr:(Vdom.Attr.class_ Styles.vbox)
           [
             Vdom.Node.h3 [ Vdom.Node.text name ];
             Dag.to_view ~collapsed ~toggle_collapsed dag;
           ]))

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    component
