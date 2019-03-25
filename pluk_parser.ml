module Stream = struct
  type 'a t = ('a stream_result) Lazy.t * int

  and 'a stream_result =
    | Item of 'a * 'a t
    | Eof

  let of_seq seq =
    let rec aux seq i () =
      match seq () with
      | Seq.Cons (a, seq) -> Item (a, (Lazy.from_fun (aux seq (i + 1)), i))
      | Seq.Nil -> Eof in
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
        Item (a, ((Lazy.from_fun next), !i))
      else
        Eof in
    Lazy.from_fun next, 0

  let of_string str = of_bytes (Bytes.of_string str)
end

type 'a stream = 'a Stream.t

type ('a, 's) item = 'a * 's stream

type 's error = string * 's stream

type ('a, 's) parse_result = (('a, 's) item, 's error) result

type ('a, 's) t = 's stream -> ('a, 's) parse_result

let apply stream parser = parser stream

let next parser proc stream =
  match (parser stream) with
  | Ok (x, s1) -> proc x s1
  | Error (err, s1) -> Error (err, s1)

let error err stream = Error (err, stream)

let any_item stream =
  match Stream.next stream with
  | Stream.Item (x, s1) -> Ok (x, s1)
  | Stream.Eof -> Error ("eof", stream)

let some_of parsers =
  match parsers with
  | [] -> failwith "Empty parsers list"
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
  let rec loop x s =
    match parser x s with
    | Ok (x, s1) -> loop x s1
    | Error _ -> Ok (x, s) in
  loop init stream

let one_or_many init parser =
  next (parser init) (fun x -> zero_or_many x parser)

let digit_of_char =
  let range = [(0, Char.code '0', Char.code '9');
               (10, Char.code 'a', Char.code 'z');
               (10, Char.code 'A', Char.code 'Z')] in
  fun x radix ->
    let rec loop x range =
      match range with
      | (i, s, e) :: range ->
          if x >= s && x <= e then begin
            let dig = (x - s) + i in
            if dig < radix then
              Some ((x - s) + i)
            else
              None
          end else
            loop x range
      | [] -> None in
    loop x range

let digit radix stream =
  match Stream.next stream with
  | Stream.Item (x, s1) ->
      begin
        match digit_of_char (Char.code x) radix with
        | Some dig -> Ok (dig, s1)
        | None -> Error ("parse_digit", s1)
      end
  | _ -> Error ("parse_digit", stream)

let number radix =
  one_or_many 0 (fun x ->
    next (digit radix) (fun dig s1 ->
      Ok ((x * radix) + dig, s1)))

let number_n radix len stream =
  let proc (i, x) s =
    if i < len then begin
      apply s (next (digit radix) (fun dig s1 ->
        Ok ((i + 1, ((x * radix) + dig)), s1)))
    end else
      Error ("parse_number_n", s) in
  apply stream (next (one_or_many (0, 0) proc) (fun (_, x) s1 ->
    Ok (x, s1)))

let exact_item item stream =
  match Stream.next stream with
  | Stream.Item (x, s1) when (x = item) -> Ok (x, s1)
  | _ -> Error ("exact_item", stream)

let match_string str test stream =
  let n = String.length str in
  let proc i s =
    if i < n then
      match Stream.next s with
      | Stream.Item (x, s1) when (test x (String.get str i)) ->
          Ok (i + 1, s1)
      | Stream.Item (_, s1) -> Error ("match_string", s1)
      | _ -> Error ("match_string premature end", s)
    else
      Error ("match_string", s) in
  match zero_or_many 0 proc stream with
  | Ok (i, s1) when (i = n) -> Ok (str, s1)
  | Ok (_, s1) -> Error ("match_string", s1)
  | Error (err, s1) -> Error (err, s1)

let exact_string str = match_string str (=)

let satisfy n_or_many test stream =
  let proc a s =
    match Stream.next s with
    | Stream.Item (x, s1) when (test x) -> Ok (a, s1)
    | _ -> Error ("match_if", s) in
  match n_or_many () proc stream with
  | Ok (a, s1) -> Ok (a, s1)
  | Error (err, s1) -> Error (err, s1)

let satisfies test stream =
  match Stream.next stream with
  | Stream.Item (x, s1) when (test x) -> Ok (x, s1)
  | _ -> Error ("match_item", stream)

let string_of parser stream =
  let buf = Buffer.create 16 in
  let rec loop s =
    match (parser s) with
    | Ok (ch, s1) ->
        Buffer.add_char buf ch;
        loop s1
    | Error _ when ((Buffer.length buf) > 0) ->
        Ok (Buffer.contents buf, s)
    | Error (err, s1) -> Error (err, s1) in
  loop stream

let string_if test stream = string_of (satisfies test) stream

let convert proc parser stream =
  match (parser stream) with
  | Ok (x, s1) -> Ok (proc x, s1)
  | Error (err, s1) -> Error (err, s1)

let void parser stream = convert ignore parser stream

let const x stream = Ok (x, stream)

let const_unit stream = const () stream

let end_of_stream stream =
  match Stream.next stream with
  | Stream.Item (_, s) -> Error ("end_of_stream", s)
  | Stream.Eof -> Ok ((), stream)
