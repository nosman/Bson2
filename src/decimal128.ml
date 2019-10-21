open Core

type t = string

let of_string s =
    if String.length s <> 16 then
        None
    else Some s

let of_string_exn s =
    let len = String.length s in
    if len <> 16 then
        failwith (Printf.sprintf "Cannot create a Decimal128 with %d bytes" len)
    else s

let to_string t =
    t
