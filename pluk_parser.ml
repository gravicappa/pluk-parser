type 'a stream_result =
  | Stream_ok of 'a * 'a stream
  | Stream_eof
and 'a stream = unit -> 'a stream_result;;

type ('a, 'b) parse_result = 
  | Parse_ok of 'a * 'b stream
  | Parse_error of string  * 'b stream;;

type ('a, 'b) t = 'b stream -> ('a, 'b) parse_result;;

type 'a cell =
  | Nil_cell
  | Eof_cell
  | Tail_cell of {mutable next: 'a cell}
  | Cell of {item: 'a; mutable next: 'a cell};;

let stream next_proc =
  let q = ref (Tail_cell {next = Nil_cell}) in

  let add cell =
    match !q with
    | Tail_cell tail ->
        tail.next <- cell;
        q := cell
    | Cell tail ->
        tail.next <- cell;
        q := cell
    | _ -> q := cell in

  let rec read_next next_proc =
    match next_proc () with
    | Some item ->
      let cell = Cell {item; next = Nil_cell} in
      add cell;
      Stream_ok (item, mk next_proc cell)
    | None ->
      add Eof_cell;
      Stream_eof

  and mk next_proc list () = 
    match list with
    | Cell {next = Cell {item; _} as next; _} ->
        Stream_ok (item, mk next_proc next)
    | Tail_cell {next = Cell {item; _} as next} -> 
        Stream_ok (item, mk next_proc next)
    | Eof_cell -> Stream_eof
    | _ -> read_next next_proc in

  mk next_proc !q;;

let stream_of_list list = 
  let list = ref list in
  stream (fun () ->
    match !list with
    | [] -> None
    | x :: xs -> 
        list := xs;
        Some x);;

let stream_of_bytes bytes offset =
  let i = ref offset in
  let n = Bytes.length bytes in
  stream (fun () ->
    if !i < n then begin
      let x = Bytes.get bytes !i in
      incr i;
      Some x
    end else
      None);;

let stream_of_string str offset =
  stream_of_bytes (Bytes.of_string str) offset;;

let stream_of_channel chan =
  stream (fun () ->
    try Some (input_char chan)
    with End_of_file -> None);;

let apply stream parser = parser stream;;

let next parser proc stream =
  match (parser stream) with
  | Parse_ok (x, s1) -> proc x s1
  | Parse_error (err, s1) -> Parse_error (err, s1);;

let error err stream = Parse_error (err, stream);;

let any_item stream =
  match stream () with
  | Stream_ok (x, s1) -> Parse_ok (x, s1)
  | Stream_eof -> Parse_error ("eof", stream);;

let some_of parsers stream =
  let rec loop = function
    | [] -> Parse_error ("parse_or", stream)
    | parser :: [] -> (parser stream)
    | parser :: parsers ->
        begin
          match (parser stream) with
          | Parse_ok (_, _) as result -> result
          | _ -> loop parsers
        end in
  loop parsers;;

let zero_or_many init parser stream =
  let rec loop x s =
    match parser x s with
    | Parse_ok (x, s1) -> loop x s1
    | Parse_error _ -> Parse_ok (x, s) in
  loop init stream;;

let one_or_many init parser =
  next (parser init) (fun x -> zero_or_many x parser);;

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
    loop x range;;

let digit radix stream =
  match stream () with
  | Stream_ok (x, s1) ->
      begin
        match digit_of_char (Char.code x) radix with
        | Some dig -> Parse_ok (dig, s1)
        | None -> Parse_error ("parse_digit", s1)
      end
  | _ -> Parse_error ("parse_digit", stream);;

let number radix =
  one_or_many 0 (fun x ->
    next (digit radix) (fun dig s1 ->
      Parse_ok ((x * radix) + dig, s1)));;

let number_n radix len stream =
  let proc (i, x) s =
    if i < len then begin
      apply s (next (digit radix) (fun dig s1 ->
        Parse_ok ((i + 1, ((x * radix) + dig)), s1)))
    end else
      Parse_error ("parse_number_n", s) in
  apply stream (next (one_or_many (0, 0) proc) (fun (_, x) s1 ->
    Parse_ok (x, s1)));;

let exact_item item stream =
  match stream () with
  | Stream_ok (x, s1) when (x = item) -> Parse_ok (x, s1)
  | _ -> Parse_error ("exact_item", stream);;

let match_string str test stream =
  let n = String.length str in
  let proc i s =
    if i < n then
      match s () with
      | Stream_ok (x, s1) when (test x (String.get str i)) ->
          Parse_ok (i + 1, s1)
      | _ -> Parse_error ("match_string", s)
    else
      Parse_error ("match_string", s) in
  match zero_or_many 0 proc stream with
  | Parse_ok (i, s1) when (i = n) -> Parse_ok (str, s1)
  | Parse_ok (_, s1) -> Parse_error ("match_string", s1)
  | Parse_error (err, s1) -> Parse_error (err, s1);;

let exact_string str = match_string str (=);;

let satisfy n_or_many test stream =
  let proc a s =
    match s () with
    | Stream_ok (x, s1) when (test x) -> Parse_ok (a, s1)
    | _ -> Parse_error ("match_if", s) in
  match n_or_many () proc stream with
  | Parse_ok (a, s1) -> Parse_ok (a, s1)
  | Parse_error (err, s1) -> Parse_error (err, s1);;

let satisfies test stream =
  match stream () with
  | Stream_ok (x, s1) when (test x) -> Parse_ok (x, s1)
  | _ -> Parse_error ("match_item", stream);;

let string_of parser stream =
  let buf = Buffer.create 16 in
  let rec loop s =
    match (parser s) with
    | Parse_ok (ch, s1) ->
        Buffer.add_char buf ch;
        loop s1
    | Parse_error _ when ((Buffer.length buf) > 0) ->
        Parse_ok (Buffer.contents buf, s)
    | Parse_error (err, s1) -> Parse_error (err, s1) in
  loop stream;;

let string_if test stream = string_of (satisfies test) stream;;

let convert proc parser stream =
  match (parser stream) with
  | Parse_ok (x, s1) -> Parse_ok (proc x, s1)
  | Parse_error (err, s1) -> Parse_error (err, s1);;

let void parser stream = convert ignore parser stream;;

let const x stream = Parse_ok (x, stream);;

let e stream = const () stream;;
