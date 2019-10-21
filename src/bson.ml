open Core

type binary_type =
    | Generic
    | Function
    | Binary_old
    | UUID_old
    | UUID
    | MD5
    | Encrypted
    | User_defined [@@deriving sexp]

type document_type = Array | Document | Js_code_w_scope of int [@@deriving sexp]

let document_type_to_char = function
    | Document -> '\x03'
    | Array -> '\x04'
    | Js_code_w_scope _ -> '\x0F'

module type BsonWriter = sig

    type t

    exception Invalid_state of string

    val create : int -> t

    val write_float : t -> string -> float -> unit

    val write_string : t -> string -> string -> unit
    
    val write_document_start : t -> string -> unit

    val write_document_close : t -> unit
    
    (* Write the start of an array. Must be closed with write_array_close. *)
    val write_array_start : t -> string -> unit

    val write_array_close : t -> unit

    val write_binary : t -> string -> binary_type -> bytes -> unit

    (* Bytes must be of length 12 *)
    val write_objectid : t -> string -> bytes -> unit

    val write_bool : t -> string -> bool -> unit
    
    val write_utc_datetime : t -> string -> int64 -> unit

    val write_null : t -> string -> unit

    val write_regex : t -> string -> pattern:string -> options:string -> unit
    
    val write_js : t -> string -> string -> unit

    (* This writes javascript to the key, then opens a new document as its scope. The document must then be closed with write_js_with_scope_close. *)
    val write_js_with_scope : t -> string -> string -> unit

    val write_js_with_scope_close : t -> unit

    val write_int32 : t -> string -> int32 -> unit
    
    val write_timestamp : t -> string -> int64 -> unit
    
    val write_int64 : t -> string -> int64 -> unit
    
    (* Bytes value must by 16 bytes long, representing a decimal128 in little-endian format. *)
    val write_decimal128 : t -> string -> bytes -> unit
    
    val write_minkey : t -> string -> unit

    val write_maxkey : t -> string -> unit

    val finalize : t -> (bytes, string) Result.t

end

module Writer : BsonWriter = struct

    type t =
        { mutable open_docs : (int * document_type) list
        ; mutable closed_docs : (int * int) list
        ; data : Buffer.t }

    exception Invalid_state of string

    let write_cstring d str =
        Buffer.add_string d.data str;
        Buffer.add_char d.data '\x00'

    let write_field c write_value d name v =
        Buffer.add_char d.data c;
        write_cstring d name;
        write_value d v

    let write_field_no_value c d name =
        Buffer.add_char d.data c;
        write_cstring d name

    let int32_to_bytes v =
        let rec helper i acc =
            if i >= 4
            then acc
            else
                let bits_to_shift = i * 8 in
                let b =
                    Int32.(shift_right v bits_to_shift land 255l
                           |> to_int_exn
                           |> Char.of_int_exn) in
                helper (i + 1) (b::acc) in
        helper 0 []
        |> List.rev

    let write_int32' d v =
        let rec helper i =
            if i >= 4
            then ()
            else
                let bits_to_shift = i * 8 in
                let b =
                    Int32.(shift_right v bits_to_shift land 255l
                           |> to_int_exn
                           |> Char.of_int_exn) in
                Buffer.add_char d.data b;
                helper (i + 1) in
        helper 0

    let write_string' d str =
        String.length str |> Int32.of_int_exn |> write_int32' d;
        Buffer.add_string d.data str;
        Buffer.add_char d.data '\x00'

    let write_string =
        write_field '\x02' write_string'

    let write_int32 =
        write_field '\x10' write_int32'

    let write_int64' d v =
        let rec helper i =
            if i >= 8
            then ()
            else
                let bits_to_shift = i * 8 in
                let b =
                    Int64.(shift_right v bits_to_shift land 255L
                           |> to_int_exn
                           |> Char.of_int_exn) in
                Buffer.add_char d.data b;
                helper (i + 1) in
        helper 0

    let write_int64 =
        write_field '\x12' write_int64'

    let write_float' d f =
        Int64.bits_of_float f
        |> write_int64' d

    let write_float =
        write_field '\x01' write_float'

    let write_bool =
        write_field '\x08'
            (fun d b ->
                let v =
                    if b then '\x01' else '\x00' in
                Buffer.add_char d.data v)

    let write_bytes size d bytes =
        let len = Bytes.length bytes in
        if len <> size
        then sprintf "Expected bytes of length %d but got bytes of length %d" size len |> failwith
        else
            Buffer.add_bytes d.data bytes

    let write_binary' subtype d bin =
        
        (* Write the size as an int32 *)
        let c =
            match subtype with
            | Generic -> '\x00'
            | Function -> '\x01'
            | Binary_old -> '\x02'
            | UUID_old -> '\x03'
            | UUID -> '\x04'
            | MD5 -> '\x05'
            | Encrypted -> '\x06'
            | User_defined -> '\x80' in
        (* Length of the binary content. *)
        Bytes.length bin
        |> Int32.of_int_exn     
        |> write_int32' d;
        
        (* Type of the binary content *)
        Buffer.add_char d.data c;
        (* The content itself. *)
        Buffer.add_bytes d.data bin

    let write_binary d name subtype bin =
        write_field '\x05' (write_binary' subtype) d name bin

    let write_timestamp =
        write_field '\x11' write_int64'

    let write_null =
        write_field_no_value '\x0A' 

    let write_minkey =
        write_field_no_value '\xFF'

    let write_maxkey =
        write_field_no_value '\x7F'

    let write_regex buf name ~pattern:p ~options:o =
        write_field_no_value '\x0B' buf name;
        write_cstring buf p;
        write_cstring buf o

    let create initial_size =
        let doc =
            { open_docs = [ 0, Document ] 
            ; closed_docs = []
            ; data = Buffer.create initial_size } in
        write_int32' doc 0l;
        doc

    let write_start' d document_type =
        let pos = Buffer.length d.data in
        write_int32' d 0l;
        d.open_docs <- (pos, document_type)::d.open_docs

    let write_start d name document_type =
        let c = document_type_to_char document_type in
        write_field_no_value c d name;
        write_start' d document_type

    let write_document_start d name =
        write_start d name Document

    let write_array_start d name =
        write_start d name Array

    let write_close d doc_type =
        match d.open_docs with
        | [] -> raise (Invalid_state "No open docs to close")
        | (start, typ)::tl ->
                if typ = doc_type
                then
                    let close = Buffer.length d.data in
                    Buffer.add_char d.data '\x00';
                    d.open_docs <- tl;
                    d.closed_docs <- (start, close)::d.closed_docs
                else 
                    let last_open = sexp_of_document_type doc_type |> string_of_sexp in
                    let try_close = sexp_of_document_type typ |> string_of_sexp in
                    let err_msg = sprintf "Cannot close %s: Open %s at position %d" try_close last_open start in
                    raise (Invalid_state err_msg)

    let write_document_close d =
        write_close d Document

    let write_array_close d =
        write_close d Array

    let write_decimal128 =
        write_field '\x13' (write_bytes 16)

    let write_js_with_scope d name jscode =
        write_field_no_value '\x0F' d name;
        let js_start = Buffer.length d.data in
        write_int32' d 0l;
        write_string' d jscode;
        let scope_start = Buffer.length d.data in
        d.open_docs <- (js_start, Js_code_w_scope scope_start)::d.open_docs;
        write_int32' d 0l

    let write_js_with_scope_close d =
        match d.open_docs with
        | [] -> raise (Invalid_state "No open docs to close")
        | (start, Js_code_w_scope scope_start)::tl ->
            let close = Buffer.length d.data in
            Buffer.add_char d.data '\x00';
            d.open_docs <- tl;
            d.closed_docs <- (start, close)::(scope_start, close)::d.closed_docs
        | (start, typ)::_ ->
            let last_open = sexp_of_document_type typ |> string_of_sexp in
            let msg = sprintf "Cannot close js code with scope: Open %s at position %d" last_open start in
            raise (Invalid_state msg)

        (* int32 string document *)
        (* Int32 is the length in bytes of the entire code_w_scope value *)
        (* Write the field. Then write the start of a document (or should it be start of js scope?) *)

    let write_js =
        write_field '\x0D' write_string'

    let write_objectid =
        write_field '\x07' (write_bytes 12)

    let write_utc_datetime =
        write_field '\x09' write_int64'

    let finalize { open_docs; closed_docs; data } =
        if List.is_empty open_docs
        then
            (* Fill in all document openings *) 
            let bytes =
                Buffer.contents_bytes data in
            List.iter
                ~f:(fun (doc_open, doc_close) ->
                    let length =
                        doc_close - doc_open + 1
                        |> Int32.of_int_exn
                        |> int32_to_bytes in
                    List.iteri length ~f:(fun i b -> Bytes.set bytes (doc_open + i) b)) closed_docs;
            Ok bytes
        else Error "Unclosed document"

end

module type BsonReader = sig

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

    type read_result =
        | Field of string * bson_type
        | End_of_document [@@deriving sexp]

    type t

    val create : bytes -> t

    val read_next : t -> read_result

end

(* Parameterize over input type. *)
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

    type read_result =
        | Field of string * bson_type
        | End_of_document [@@deriving sexp]

    type t =
        { mutable pos : int
        ; data: bytes }

    let read_int32 ({ data; pos } as d) =
        let last = pos + 4 in
        let s = Bytes.sub data ~pos:pos ~len:4 in
        let rec helper i acc =
            if i < 0
            then acc
            else
                let high =
                    Bytes.get s i
                    |> Char.to_int
                    |> Int32.of_int_exn in
                let acc =
                    Int32.(shift_left acc 8 lor high) in
                helper (i - 1) acc in
        d.pos <- last;
        helper 3 0l

        let read_int64 ({ data; pos } as d) =
            let last = pos + 8 in
        let s = Bytes.sub data ~pos:pos ~len:8 in
        let rec helper i acc =
            if i < 0
            then acc
            else
                let high =
                    Bytes.get s i
                    |> Char.to_int
                    |> Int64.of_int in
                let acc =
                    Int64.(shift_left acc 8 lor high) in
                helper (i - 1) acc in
        d.pos <- last;
        helper 7 0L

    let read_float d =
        read_int64 d
        |> Int64.float_of_bits

    let read_string d =
        let size = read_int32 d in
        let str =
            Bytes.sub d.data ~pos:d.pos ~len:(Int32.to_int_exn size)
            |> Bytes.to_string in
        (* + 1 because of the null byte at the end. TODO: validate that this byte is null *)
        d.pos <- d.pos + (Int32.to_int_exn size) + 1; 
        str

    let read_cstring ({ data; pos} as d) =
        (* Iterate until a null byte is reached *)
        (* TODO: catch out of bounds exception *)
        let rec helper curr =
            if Bytes.get data (curr + pos) = '\x00'
            then curr
            else
                curr + 1 |> helper in
        let len = helper 0 in
        let str =
            Bytes.sub data ~pos:pos ~len:len
            |> Bytes.to_string in
        let pos' = pos + len + 1 in
        d.pos <- pos';
        str
    
    (* TODO: validate size. *)
    let create bytes =
        let d =
            { pos = 0
            ; data = bytes } in
        let _size = read_int32 d in
        d

    let read_document_start d =
        let _size = read_int32 d in
        Document_start

    let read_array_start d =
        let _size = read_int32 d in
        Array_start

    let read_name ({ data; pos } as d) =
        let c = Bytes.get data pos in
        d.pos <- pos + 1;
        let name = read_cstring d in
        c, name

    let read_bool ({ pos; data } as d) =
        let b =
            match Bytes.get data pos with
            | '\x00' -> false
            | '\x01' -> true
            | c -> sprintf "Invalid value %c for boolean at position %d" c pos |> failwith in
        d.pos <- d.pos + 1;
        b

    let read_binary d =
        let size = read_int32 d |> Int32.to_int_exn in
        let binary_type =
            match Bytes.get d.data d.pos with
            | '\x00' -> Generic
            | '\x01' -> Function
            | '\x02' -> Binary_old
            | '\x03' -> UUID_old
            | '\x04' -> UUID
            | '\x05' -> MD5
            | '\x06' -> Encrypted
            | '\x80' -> User_defined
            | x -> sprintf "Invalid binary subtype %c at position %d" x d.pos |> failwith in
        d.pos <- d.pos + 1;
        let bin =
             Bytes.sub d.data ~pos:d.pos ~len:size in
        d.pos <- d.pos + size;
        Binary(binary_type, bin)
    
    let read_bytes len d =
        let result = Bytes.sub d.data ~pos:d.pos ~len:len in
        d.pos <- d.pos + len;
        result

    let read_objectid =
        read_bytes 12

    let read_decimal128 =
        read_bytes 16

    let read_regex d =
        let pattern = read_cstring d in
        let options = read_cstring d in
        Regex { pattern; options }

    let read_js_with_scope d =
        let _size = read_int32 d in
        let code = read_string d in
        read_document_start d |> ignore;
        JSCode_with_scope code
    
    let read_next ({ pos; data } as d) =
        match Bytes.get data pos with
        | '\x00' -> d.pos <- d.pos + 1; End_of_document
        | _ ->
            try
            begin
                let old_pos = d.pos in
                let c, name = read_name d in
                match c with
                | '\x01' -> Field(name, Double(read_float d))
                | '\x02' -> Field(name, String(read_string d))
                | '\x03' -> Field(name, read_document_start d)
                | '\x04' -> Field(name, read_array_start d)
                | '\x05' -> Field(name, read_binary d)
                | '\x07' -> Field(name, ObjectId(read_objectid d))
                | '\x08' -> Field(name, Boolean(read_bool d))
                | '\x09' -> Field(name, DateTime(read_int64 d))
                | '\x0A' -> Field(name, Null)
                | '\x0B' -> Field(name, read_regex d)
                | '\x0D' -> Field(name, JSCode(read_string d))
                | '\x0F' -> Field(name, read_js_with_scope d)
                | '\x10' -> Field(name, Int32(read_int32 d))
                | '\x11' -> Field(name, Timestamp(read_int64 d))
                | '\x12' -> Field(name, Int64(read_int64 d))
                | '\x13' -> Field(name, Decimal128(read_decimal128 d))
                | '\xFF' -> Field(name, Min_key) 
                | '\x7F' -> Field(name, Max_key)
                | c -> failwith (sprintf "TODO: code %d pos:%d" (Char.to_int c) old_pos)
            end
            with x -> (*sprintf "Unexpected exn at position %d" d.pos |> failwith*)
                raise x

end
