(* Pluk-parser
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

(**

A simple parser.

 *)

type 'a stream_result =
  | Stream_ok of 'a * 'a stream
  | Stream_eof
(** Stream result type *)

and 'a stream = unit -> 'a stream_result
(** Stream type *)

type ('a, 'b) parse_result =
  | Parse_ok of 'a * 'b stream
  | Parse_error of string * 'b stream
(** Parse result type *)

type ('a, 'b) t = 'b stream -> ('a, 'b) parse_result
(** Parser function type *)

val stream : (unit -> 'a option) -> 'a stream

val stream_of_list : 'a list -> 'a stream

val stream_of_bytes : bytes -> int -> char stream

val stream_of_string : string -> int -> char stream

val stream_of_channel : in_channel -> char stream

val next :
  ('a stream -> ('b, 'c) parse_result) ->
  ('b -> 'c stream -> ('d, 'c) parse_result) ->
  'a stream -> ('d, 'c) parse_result

val error : string -> 'b stream -> ('a, 'b) parse_result

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

val e : 'a stream -> (unit, 'a) parse_result
