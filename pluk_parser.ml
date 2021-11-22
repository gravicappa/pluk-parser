module Stream = struct
  type 'a t = ('a stream_result) Lazy.t * int

  and 'a stream_result =
    | Value of 'a * 'a t
    | End

  let of_seq seq =
    let rec aux seq i () =
      match seq () with
      | Seq.Cons (a, seq) -> Value (a, (Lazy.from_fun (aux seq (i + 1)), i))
      | Seq.Nil -> End in
    Lazy.from_fun (aux seq 1), 0

  let next ((lazy proc, _) : 'a t) = proc
  let position ((_, pos) : 'a t) = pos

  let of_bytes bytes =
    let i = ref 0 in
    let n = Bytes.length bytes in
    let rec next () =
      if !i < n then
        let a = Bytes.get bytes !i in
        let () = incr i in
        Value (a, ((Lazy.from_fun next), !i))
      else
        End in
    Lazy.from_fun next, 0

  let of_string str = of_bytes (Bytes.of_string str)
end

type msg = ..

type msg +=
  | Eof
  | Invalid_digit_character
  | Invalid_digit
  | Invalid_number_character
  | Exact_item_mismatch
  | String_mismatch
  | Inexpected_end_of_stream
  | Satisfy_mismatch
  | Item_mismatch
  | Stream_not_ended

type 'a stream = 'a Stream.t

type ('a, 's) elt = 'a * 's stream

type 's error = (msg, 's) elt

type ('a, 's) parse_result = (('a, 's) elt, 's error) result

type ('a, 's) t = 's stream -> ('a, 's) parse_result

let apply stream parser = parser stream

let map proc = function
  | Ok (a, stream) -> Ok (proc a, stream)
  | Error e -> Error e

let map_error proc = function
  | Ok a -> Ok a
  | Error (e, stream) -> Error (proc e, stream)

let bind a proc =
  match a with
  | Ok a -> proc a
  | Error e -> Error e

let ok a stream = Ok (a, stream)

let error err stream = Error (err, stream)

let map_value proc parser stream = map proc (parser stream)

let ok_unit stream = ok () stream

let next parser proc stream =
  match (parser stream) with
  | Ok (a, stream') -> proc a stream'
  | Error e -> Error e

let string_of_msg = function
  | Eof -> Some "Eof"
  | Invalid_digit_character -> Some "Invalid_digit_character"
  | Invalid_digit -> Some "Invalid_digit"
  | Invalid_number_character -> Some "Invalid_number_character"
  | Exact_item_mismatch -> Some "Exact_item_mismatch"
  | String_mismatch -> Some "String_mismatch"
  | Inexpected_end_of_stream -> Some "Inexpected_end_of_stream"
  | Satisfy_mismatch -> Some "Satisfy_mismatch"
  | Item_mismatch -> Some "Item_mismatch"
  | Stream_not_ended -> Some "Stream_not_ended"
  | _ -> None

let any_item stream =
  match Stream.next stream with
  | Stream.Value (x, s1) -> Ok (x, s1)
  | Stream.End -> Error (Eof, stream)

let or_ parsers =
  match parsers with
  | [] -> invalid_arg "Empty parsers list"
  | p :: ps ->
      fun stream ->
        let rec parse parser parsers =
          match (parser stream) with
          | Ok _ as result -> result
          | err -> loop err parsers

        and loop err = function
          | [] -> err
          | p :: ps -> parse p ps in

        parse p ps

let zero_or_many init parser stream =
  let rec loop a stream =
    match parser a stream with
    | Ok (a, stream') -> loop a stream'
    | Error _ -> Ok (a, stream) in
  loop init stream

let one_or_many init parser stream =
  bind (parser init stream) @@ fun (a, stream) ->
    zero_or_many a parser stream

let digit_of_char =
  let range = [(0, Char.code '0', Char.code '9');
               (10, Char.code 'a', Char.code 'z');
               (10, Char.code 'A', Char.code 'Z')] in
  fun a radix ->
    let rec loop a range =
      match range with
      | (base, range_a, range_b) :: range ->
          if a >= range_a && a <= range_b then begin
            let digit = (a - range_a) + base in
            if digit < radix then
              Some ((a - range_a) + base)
            else
              None
          end else
            loop a range
      | [] -> None in
    loop a range

let digit radix stream =
  match Stream.next stream with
  | Stream.Value (a, stream') ->
      begin match digit_of_char (Char.code a) radix with
      | Some digit -> Ok (digit, stream')
      | None -> Error (Invalid_digit_character, stream')
      end
  | _ -> Error (Invalid_digit, stream)

let number radix =
  one_or_many 0 (fun a ->
    next (digit radix) (fun digit stream ->
      Ok ((a * radix) + digit, stream)))

let number_n radix len stream =
  let proc (i, x) stream =
    if i < len then begin
      apply stream (next (digit radix) (fun digit stream' ->
        Ok ((i + 1, ((x * radix) + digit)), stream')))
    end else
      Error (Invalid_number_character, stream) in

  apply stream (next (one_or_many (0, 0) proc) (fun (_, a) stream' ->
    Ok (a, stream')))

let exact_item item stream =
  match Stream.next stream with
  | Stream.Value (a, stream') when (a = item) -> Ok (a, stream')
  | _ -> Error (Exact_item_mismatch, stream)

let match_string str test stream =
  let n = String.length str in
  let proc i stream =
    if i < n then
      match Stream.next stream with
      | Stream.Value (a, stream') when (test a (String.get str i)) ->
          Ok (i + 1, stream')
      | Stream.Value (_, stream') -> Error (String_mismatch, stream')
      | _ -> Error (Inexpected_end_of_stream, stream)
    else
      Error (String_mismatch, stream) in
  match zero_or_many 0 proc stream with
  | Ok (i, stream') when (i = n) -> Ok (str, stream')
  | Ok (_, stream') -> Error (String_mismatch, stream')
  | Error err -> Error err

let exact_string str = match_string str (=)

let satisfy n_or_many test stream =
  let proc a stream =
    match Stream.next stream with
    | Stream.Value (b, stream') when test b -> Ok (a, stream')
    | _ -> Error (Satisfy_mismatch, stream) in

  match n_or_many () proc stream with
  | Ok (a, stream') -> Ok (a, stream')
  | Error err -> Error err

let satisfies test stream =
  match Stream.next stream with
  | Stream.Value (a, stream') when test a -> Ok (a, stream')
  | _ -> Error (Item_mismatch, stream)

let into_string parser stream =
  let buf = Buffer.create 16 in
  let rec loop stream =
    match (parser stream) with
    | Ok (ch, stream') ->
        Buffer.add_char buf ch;
        loop stream'
    | Error _ when ((Buffer.length buf) > 0) ->
        Ok (Buffer.contents buf, stream)
    | Error err -> Error err in
  loop stream

let into_string_if test stream = into_string (satisfies test) stream

let end_of_stream stream =
  match Stream.next stream with
  | Stream.Value (_, stream') -> Error (Stream_not_ended, stream')
  | Stream.End -> Ok ((), stream)
