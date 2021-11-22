(** Simple combinatorial parser *)

(**
  Stream is a source of data for parser. Each instance of {!Stream.t} holds on
  a specific position in underlying source. Multiple invocation of
  {!Stream.next} on some {!Stream.t} instance always returns the same result.
*)
module Stream : sig
  type 'a t
  (** The type for stream *)

  and 'a stream_result =
    | Value of 'a * 'a t
    | End
  (** The type for result of {!next}. *)

  val next : 'a t -> 'a stream_result
  (** [next stream] returns the value from [stream] and instance of
      stream positioned at next value *)

  val position : 'a t -> int
  (** [position stream] returns the position of [stream] *)

  val of_seq : 'a Seq.t -> 'a t
  (** [of_seq seq] creates a stream using [seq] as a source. *)

  val of_bytes : bytes -> char t
  (** [of_bytes bytes] creates a stream using [bytes] as a source. *)

  val of_string : string -> char t
  (** [of_string string] creates a stream using [string] as a source. *)
end

type 'a stream = 'a Stream.t

type ('a, 's) elt = 'a * 's stream

type msg = ..
(** The type for error messages. *)

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
(** Extended type for error messages returned by the library functions. *)

type 's error = (msg, 's) elt

type ('a, 's) parse_result = (('a, 's) elt, 's error) result
(** Type for return value of each parser *)

type ('a, 's) t = 's stream -> ('a, 's) parse_result
(** Type of parser *)

val apply : 's stream -> ('a, 's) t -> ('a, 's) parse_result
(** [apply stream parser] returns the result of applying [parser] on [stream] *)

val ok : 'a -> 'b stream -> ('a, 'b) parse_result

val error : msg -> 's stream -> ('a, 's) parse_result

val string_of_msg: msg -> string option

val map: ('a -> 'b) -> ('a, 's) parse_result -> ('b, 's) parse_result

val map_error: (msg -> msg) -> ('a, 's) parse_result -> ('a, 's) parse_result

val bind:
  ('a, 's) parse_result ->
  (('a * 's stream) -> ('b, 's) parse_result) ->
  ('b, 's) parse_result

val map_value :
  ('a -> 'b) -> ('c -> ('a, 'd) parse_result) -> 'c -> ('b, 'd) parse_result

val ok_unit : 'a stream -> (unit, 'a) parse_result

val next :
  ('a, 's) t ->
  ('a -> 's stream -> ('b, 's) parse_result) ->
  's stream ->
  ('b, 's) parse_result
(** [next parser proc stream] applies [proc] on result of [apply parse stream].
  It is used for parser chaining:
    {[
      let parse_key_value =
        next parse_word @@ fun key ->
          next parse_delimiter @@ fun () ->
            next parse_value @@ fun value stream' ->
              Ok ((key, value), stream')
    ]}
 *)

val any_item : 'a stream -> ('a, 'a) parse_result

val or_ :
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

val into_string :
  ('a stream -> (char, 'a) parse_result) ->
  'a stream ->
  (string, 'a) parse_result

val into_string_if :
  (char -> bool) -> char stream -> (string, char) parse_result

val end_of_stream : 'a stream -> (unit, 'a) parse_result
