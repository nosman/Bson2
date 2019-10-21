type t

val create : bytes -> t option

val create_exn : bytes -> t

val to_bytes : t -> bytes
