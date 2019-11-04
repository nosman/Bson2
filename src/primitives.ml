open Core

let decode_int32 read_bytes =
    let b = read_bytes 4 in
    let rec helper i acc =
        if i < 0 
        then acc
        else
            let high =
                Bytes.get b i
                |> Char.to_int
                |> Int32.of_int_exn in
            let acc =
                Int32.(shift_left acc 8 lor high) in
            helper (i - 1) acc in
    helper 3 0l

let decode_int64 read_bytes =
    let b = read_bytes 8 in
    let rec helper i acc =
        if i < 0 
        then acc
        else
            let high =
                Bytes.get b i
                |> Char.to_int
                |> Int64.of_int in
            let acc =
                Int64.(shift_left acc 8 lor high) in
            helper (i - 1) acc in
    helper 7 0L

let decode_cstring reader =
    let buf = Buffer.create 80 in
    let rec helper () =
        match reader () with
        | '\x00' -> Buffer.contents buf
        | c -> Buffer.add_char buf c; helper () in
    helper ()

let encode_int64 buf v =
    let rec helper i =
        if i >= 8
        then ()
        else
            let bits_to_shift = i * 8 in
            let b =
                Int64.(shift_right v bits_to_shift land 255L
                       |> to_int_exn
                       |> Char.of_int_exn) in
            Buffer.add_char buf b;
            helper (i + 1) in
    helper 0

let encode_int32 buf v =
    let rec helper i =
        if i >= 4
        then ()
        else
            let bits_to_shift = i * 8 in
            let b =
                Int32.(shift_right v bits_to_shift land 255l
                       |> to_int_exn
                       |> Char.of_int_exn) in
            Buffer.add_char buf b;
            helper (i + 1) in
    helper 0

let encode_cstring buf str =
    Buffer.add_string buf str;
    Buffer.add_char buf '\x00'
