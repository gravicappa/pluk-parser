module Stream : sig
  type 'a t = 'a stream_result Lazy.t * int
  and 'a stream_result = Item of 'a * 'a t | Eof
  val of_seq : 'a Seq.t -> 'a t
  val next : 'a t -> 'a stream_result
  val position : 'a t -> int
  val of_bytes : bytes -> char t
  val of_string : string -> char t
end

type 'a stream = 'a Stream.t
type ('a, 's) item = 'a * 's stream
type 's error = string * 's stream
type ('a, 's) parse_result = (('a, 's) item, 's error) result
type ('a, 's) t = 's stream -> ('a, 's) parse_result

val apply : 's stream -> ('a, 's) t -> ('a, 's) parse_result

val next : ('a, 's) t -> ('a -> 's stream -> ('b, 's) parse_result)
           -> 's stream -> ('b, 's) parse_result

val error : string -> 's stream -> ('a, 's) parse_result

val any_item : 'a stream -> ('a, 'a) parse_result

val some_of :
  ('a stream -> ('b, 'a) parse_result) list ->
  'a stream -> ('b, 'a) parse_result

val zero_or_many :
  'a ->
  ('a -> 'b stream -> ('a, 'b) parse_result) ->
  'b stream -> ('a, 'b) parse_result

val one_or_many :
  'a ->
  ('a -> 'b stream -> ('a, 'b) parse_result) ->
  'b stream -> ('a, 'b) parse_result

val digit_of_char : int -> int -> int option

val digit : int -> char stream -> (int, char) parse_result

val number : int -> char stream -> (int, char) parse_result

val number_n : int -> int -> char stream -> (int, char) parse_result

val exact_item : 'a -> 'a stream -> ('a, 'a) parse_result

val match_string : string -> ('a -> char -> bool) ->
                   'a stream -> (string, 'a) parse_result

val exact_string : string -> char stream -> (string, char) parse_result

val satisfy :
  (unit ->
   ('a -> 'b stream -> ('a, 'b) parse_result) ->
   'c -> ('d, 'e) parse_result) ->
  ('b -> bool) -> 'c -> ('d, 'e) parse_result

val satisfies : ('a -> bool) -> 'a stream -> ('a, 'a) parse_result

val string_of :
  ('a stream -> (char, 'a) parse_result) ->
  'a stream -> (string, 'a) parse_result

val string_if : (char -> bool) -> char stream -> (string, char) parse_result

val convert :
  ('a -> 'b) -> ('c -> ('a, 'd) parse_result) -> 'c -> ('b, 'd) parse_result

val void : ('a -> ('b, 'c) parse_result) -> 'a -> (unit, 'c) parse_result

val const : 'a -> 'b stream -> ('a, 'b) parse_result

val const_unit : 'a stream -> (unit, 'a) parse_result

val end_of_stream : 'a stream -> (unit, 'a) parse_result
