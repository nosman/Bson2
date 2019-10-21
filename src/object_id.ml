type t = bytes

let create bytes =
    if Bytes.length bytes = 12 then
        Some bytes
    else
        None

let create_exn bytes =
    if Bytes.length bytes = 12 then
        bytes
    else
        failwith "Object id must be 12 bytes long"

let to_bytes t : bytes =
    Bytes.copy t 


        
