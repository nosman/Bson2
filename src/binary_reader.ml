open Core
open S
open Primitives

module Reader : BsonReader = struct

    type bson_type =
        | Double of float
        | String of string
        | Document_start
        | Array_start
        | Binary of binary_type * bytes
        | ObjectId of bytes 
        | Boolean of bool
        | DateTime of int64
        | Null
        | Regex of { pattern:string;  options:string } (* Options must be stored in alphabetical order *)
        | JSCode of string
        | JSCode_with_scope of string
        | Int32 of int32
        | Timestamp of int64 
        | Int64 of int64
        | Decimal128 of bytes 
        | Min_key
        | Max_key [@@deriving sexp]

    exception No_data of string

    type read_result =
        | Field of string * bson_type
        | End_of_document [@@deriving sexp]

    type t =
        { read_bytes: int -> bytes
        ; read_char: unit -> char }

    let read_float d =
        decode_int64 d.read_bytes
        |> Int64.float_of_bits

    let read_string d =
        let size = decode_int32 d.read_bytes in
        let str =
            d.read_bytes (Int32.to_int_exn size)
            |> Bytes.to_string in
        match d.read_char () with
        | '\x00' -> str
        | c -> failwithf "Malformed document: string terminated with %c instead of null character." c ()
    
    let read_document_start d =
        let _size = decode_int32 d.read_bytes in
        Document_start

    let read_array_start d =
        let _size = decode_int32 d.read_bytes in
        Array_start

    let read_bool d =
        match d.read_char () with
        | '\x00' -> false
        | '\x01' -> true
        | c -> failwithf "Invalid value %c for boolean" c () 

    let read_binary d =
        let size = decode_int32 d.read_bytes |> Int32.to_int_exn in
        let binary_type =
            S.binary_type_of_char (d.read_char ()) in 
        let bin = d.read_bytes size in
        Binary(binary_type, bin)

    let read_objectid d =
        d.read_bytes 12

    let read_decimal128 d =
        d.read_bytes 16

    let read_regex d =
        let pattern = decode_cstring d.read_char in
        let options = decode_cstring d.read_char in
        Regex { pattern; options }

    let read_js_with_scope d =
        let _size = decode_int32 d.read_bytes in
        let code = read_string d in
        read_document_start d |> ignore;
        JSCode_with_scope code
    
    let read_next d =
        match d.read_char () with
        | '\x00' -> End_of_document
        | c ->
            try
            begin
                let name = decode_cstring d.read_char in
                match c with
                | '\x01' -> Field(name, Double(read_float d))
                | '\x02' -> Field(name, String(read_string d))
                | '\x03' -> Field(name, read_document_start d)
                | '\x04' -> Field(name, read_array_start d)
                | '\x05' -> Field(name, read_binary d)
                | '\x07' -> Field(name, ObjectId(read_objectid d))
                | '\x08' -> Field(name, Boolean(read_bool d))
                | '\x09' -> Field(name, DateTime(decode_int64 d.read_bytes))
                | '\x0A' -> Field(name, Null)
                | '\x0B' -> Field(name, read_regex d)
                | '\x0D' -> Field(name, JSCode(read_string d))
                | '\x0F' -> Field(name, read_js_with_scope d)
                | '\x10' -> Field(name, Int32(decode_int32 d.read_bytes))
                | '\x11' -> Field(name, Timestamp(decode_int64 d.read_bytes))
                | '\x12' -> Field(name, Int64(decode_int64 d.read_bytes))
                | '\x13' -> Field(name, Decimal128(read_decimal128 d))
                | '\xFF' -> Field(name, Min_key) 
                | '\x7F' -> Field(name, Max_key)
                | c -> failwithf "TODO: name %s, code %d" name (Char.to_int c) ()
            end
            with x -> (*sprintf "Unexpected exn at position %d" d.pos |> failwith*)
                raise x

    let of_bytes' b =
        let pos = ref 0 in
        let read_char () =
            if !pos >= Bytes.length b
            then failwithf "Reached end of file at position %d" !pos () 
            else
                let i = !pos in
                let c = Bytes.get b i in
                pos := i + 1;
                c in
        let read_bytes n =
            let b = Bytes.sub b ~pos:!pos ~len:n in
            pos := !pos + n;
            b in
        { read_char; read_bytes }

    let of_bytes b =
        let reader = of_bytes' b in
        let _size = decode_int32 reader.read_bytes in
        reader

    let of_string s =
        let bytes = Bytes.of_string s in
        of_bytes bytes

    let of_in_channel' c =
        let read_bytes n =
            let b = Bytes.create n in
            let bytes_read = Stdio.In_channel.input c ~buf:b ~pos:0 ~len:n in
            if bytes_read = n
            then
                b
            else
                let pos = Stdio.In_channel.pos c in
                No_data(sprintf "No data at position %Ld" pos) |> raise in
        { read_char =
            (fun () ->
                match Stdio.In_channel.input_char c with
                | Some c -> c
                | None ->
                    let pos = Stdio.In_channel.pos c in
                    No_data(sprintf "No data at position %Ld" pos) |> raise)
        ; read_bytes = read_bytes }

    let of_in_channel c =
        let reader = of_in_channel' c in
        let _size = decode_int32 reader.read_bytes in
        reader
end
