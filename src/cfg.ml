open Core_kernel

module type S = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val hash : t -> int
end

module Make (Vertex : S) = struct
  include Graph.Persistent.Digraph.Concrete (Vertex)

  let analyze_dataflow ?(rev = false) cfg ~transfer ~bottom ~join ~equal =
    let mapping = Hashtbl.create (module Vertex) in
    let worklist = Queue.create () in
    iter_vertex
      (fun v ->
        Hashtbl.set mapping ~key:v ~data:bottom;
        Queue.enqueue worklist v )
      cfg;
    let f_fold, f_iter =
      if rev then (fold_succ, iter_pred) else (fold_pred, iter_succ)
    in
    let rec loop () =
      match Queue.dequeue worklist with
      | None -> ()
      | Some node ->
          let input =
            f_fold
              (fun pred state -> join state (Hashtbl.find_exn mapping pred))
              cfg node bottom
          in
          let output = transfer node input in
          if not (equal output (Hashtbl.find_exn mapping node)) then (
            Hashtbl.set mapping ~key:node ~data:output;
            f_iter (Queue.enqueue worklist) cfg node );
          loop ()
    in
    loop (); mapping
end
