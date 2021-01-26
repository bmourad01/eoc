open Core_kernel

include module type of Graph.Persistent.Digraph.Concrete (Label)

val analyze_dataflow :
     ?rev:bool
  -> t
  -> transfer:(vertex -> 'a -> 'a)
  -> bottom:'a
  -> join:('a -> 'a -> 'a)
  -> equal:('a -> 'a -> bool)
  -> (vertex, 'a) Hashtbl.t
