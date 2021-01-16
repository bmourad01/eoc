open Core_kernel

type t = string [@@deriving equal, compare, hash, sexp]

module Map = String.Map
module Set = String.Set
