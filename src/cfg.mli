open Core_kernel

module type S = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val hash : t -> int
end

module Make : functor (Vertex : S) -> sig
  include module type of Graph.Persistent.Digraph.Concrete (Vertex)

  val analyze_dataflow :
       ?rev:bool
    -> t
    -> transfer:(vertex -> 'a -> 'a)
    -> bottom:'a
    -> join:('a -> 'a -> 'a)
    -> equal:('a -> 'a -> bool)
    -> (vertex, 'a) Hashtbl.t
end
