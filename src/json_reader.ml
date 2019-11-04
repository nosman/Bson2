open Core
(*open Yojson *)
open Binary_reader.Reader

(* TODO: replace `String with `IntLit where appropriate *)
let read_json reader =
    let rec helper acc =
        match read_next reader with
        | Field(name, Document_start) ->
            let fields =
                helper []
                |> List.rev in 
            helper ((name, `Assoc fields)::acc)
        | Field(name, Array_start) ->
            let lst =
                helper []
                |> List.rev_map ~f:snd in
            helper ((name, `List lst)::acc)
        | Field(name, JSCode_with_scope code) ->
            let scope =
                helper []
                |> List.rev in
            helper ((name, `Assoc [ "$code", `String code; "$scope", `Assoc scope ])::acc)
        | Field(name, Double f) ->
            helper ((name, `Float f)::acc)
        | Field(name, String str) ->
            helper ((name, `String str)::acc)
        | Field(name, Binary(t, b)) ->
            let subtype = sprintf "%c" (S.binary_type_to_char t) in
            helper ((name, `Assoc [ "$binary", `Assoc [ "base64", `String (Bytes.to_string b); "subtype", `String subtype ] ])::acc)
        | Field(name, ObjectId o) ->
            let oid = Bytes.to_string o in
            helper ((name, `Assoc [ "$oid", `String oid ])::acc)
        | Field(name, Boolean b) ->
            helper ((name, `Bool b)::acc)
        | Field(name, DateTime d) ->
            helper ((name, `Assoc ["$date", `Assoc ["$numberLong",  `Intlit(sprintf "%Ld" d) ]])::acc)
        | Field(name, Null) ->
            helper ((name, `Null)::acc)
        | Field(name, Regex { pattern;  options }) ->
            helper ((name, `Assoc [ "$regularExpression", `Assoc [ "pattern", `String pattern; "options", `String options ]])::acc)
        | Field(name, JSCode code) ->
            helper ((name, `Assoc [ "$code", `String code ])::acc)
        | Field(name, Int32 i) ->
                helper ((name, `Assoc [ "$numberInt", `Intlit(sprintf "%ld" i) ])::acc)
        | Field(name, Timestamp t) ->
                (* TODO: figure out what the increment is *)
                helper ((name, `Assoc [ "$timeStamp", `Assoc [ "t", `Int (Int64.to_int_exn t); "i", `Int 1]])::acc)
        | Field(name, Int64 i) ->
            helper ((name, `Assoc [ "$numberLong", `Intlit(sprintf "%Ld" i) ])::acc)
        | Field(name, Decimal128 b) ->
            helper ((name, `Assoc [ "$numberDecimal", `String(Bytes.to_string b) ])::acc)
        | Field(name, Min_key) ->
            helper ((name, `Assoc [ "$minKey", `Int 1])::acc)
        | Field(name, Max_key) ->
            helper ((name, `Assoc [ "$maxKey", `Int 1])::acc)
        | End_of_document -> acc in
    `Assoc(List.rev (helper []))

(*
let rec read_json reader =
    match Reader.read_next reader with
    | Field(name, Double f) -> `Float f
    | Field(name, String str) -> `String str
    | Field(name, Document_start) ->
        let rec helper reader acc =
            match Reader.read_next reader with
            | Field(name, v) ->
                let json = read_json reader in
                helper reader ((name, )
    | Array_start ->
        let helper reader acc -
            match Reader.read_next reader with
            | End_of_document -> acc
            | x
    | Binary(t, b) ->
        let subtype = sprinf "%c" (S.binary_type_to_char b) in
        `Assoc [ "$binary", `Assoc [ "base64", `String (Bytes.to_string b); "subtype", `String subtype ] ]
    | ObjectId o ->
        let oid = Bytes.to_string o in
        `Assoc [ "$oid", oid ]
    | Boolean b -> `Bool b
    | DateTime d -> `Assoc ["$date", `Assoc ["$numberLong",  `String(sprintf "%Ld" d) ]]
    | Null -> `Null
    | Regex { pattern;  options } ->
            | JSCode code -> `Assoc [ "$code", `String code ]
    | JSCode_with_scope code ->
    | Int32 i -> 
    | Timestamp t
    | Int64 i ->
    | Decimal128 b ->
    | Min_key ->
    | Max_key -> *)
