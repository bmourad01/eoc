open Core_kernel
include Graph.Persistent.Digraph.Concrete (Label)

let analyze_dataflow cfg ~transfer ~bottom ~join ~equal =
  let mapping = Hashtbl.create (module Label) in
  let worklist = Queue.create () in
  iter_vertex
    (fun v ->
      Hashtbl.set mapping v bottom;
      Queue.enqueue worklist v)
    cfg;
  let trans_cfg =
    let trans_cfg = fold_vertex (fun v acc -> add_vertex acc v) cfg empty in
    fold_edges (fun u v acc -> add_edge acc v u) cfg trans_cfg
  in
  let rec loop () =
    match Queue.dequeue worklist with
    | None -> ()
    | Some node ->
        let input =
          fold_pred
            (fun pred state -> join state (Hashtbl.find_exn mapping pred))
            trans_cfg node bottom
        in
        let output = transfer node input in
        if not (equal output (Hashtbl.find_exn mapping node)) then (
          Hashtbl.set mapping node output;
          iter_pred (Queue.enqueue worklist) cfg node );
        loop ()
  in
  loop (); mapping
