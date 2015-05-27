%%
%% Copyright 2015 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(jsondoc_decode).

-define(IS_SPACE(C), (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).
-define(IS_NUMBER_TERMINATOR(C), (C =:= $} orelse C =:= $, orelse C =:= $])).

%% ====================================================================
%% API functions
%% ====================================================================
-export([decode/1]).

decode(JSon) when is_binary(JSon) -> 
	case search(JSon) of
		nil -> erlang:error(no_json);
		{Other, Rest} -> 
			case search(Rest) of
				nil -> Other;
				_ -> erlang:error(invalid_json)
			end
	end;
decode(JSon) when is_list(JSon) ->
	decode(list_to_binary(JSon)).

%% ====================================================================
%% Internal functions
%% ====================================================================

search(<<${, T/binary>>) -> 
	object(T, {[]});
search(<<$[, T/binary>>) -> 
	array(T, []);
search(<<C, T/binary>>) when ?IS_SPACE(C) -> 
	search(T);
search(<<Char, T/binary>>) -> 
	value(T, Char);
search(<<>>) -> nil.

object(<<>>, _Obj) ->
	erlang:error(object_not_closed);
object(<<$}, T/binary>>, Obj) -> {reverse(Obj), T};
object(<<",", T/binary>>, Obj) -> 
	object(T, Obj);
object(<<C, T/binary>>, Obj) when ?IS_SPACE(C) -> 
	object(T, Obj);
object(T, Obj) ->
	case search_for(T, $") of
		false -> erlang:error(invalid_object);
		{true, Rest} -> 
			{Key, Rest1} = string(Rest, []),
			case search_for(Rest1, $:) of
				false -> erlang:error(invalid_object);
				{true, Rest2} -> 
					case search(Rest2) of
						nil -> erlang:error(array_not_closed);
						{Value, Rest3} -> 
							object(Rest3, append_key_value(Obj, Key, Value))
					end
			end
	end.

reverse({List}) -> {lists:reverse(List)}.

append_key_value({List}, Key, Value) -> {[{Key, Value}|List]}.

search_for(<<C, T/binary>>, C) -> {true, T};
search_for(<<C, T/binary>>, S) when ?IS_SPACE(C) ->
	search_for(T, S);
search_for(_, _) -> false.

array(<<>>, _Acc) ->
	erlang:error(array_not_closed);
array(<<$], T/binary>>, Acc) -> {lists:reverse(Acc), T};
array(<<",", T/binary>>, Acc) -> 
	array(T, Acc);
array(<<C, T/binary>>, Acc) when ?IS_SPACE(C) -> 
	array(T, Acc);
array(T, Acc) ->
	case search(T) of
		nil -> erlang:error(array_not_closed);
		{Other, Rest} -> array(Rest, [Other|Acc])
	end.

value(<<"rue", T/binary>>, $t) -> {true, T};
value(<<"alse", T/binary>>, $f) -> {false, T};
value(<<"ull", T/binary>>, $n) -> {null, T};
value(Value, $") -> string(Value, []);
value(Value, Char) -> number(Value, [Char]).

string(<<$", T/binary>>, Acc) -> 
	{iolist_to_binary(lists:reverse(Acc)), T};
string(<<"\\\"", T/binary>>, Acc) -> 
	string(T, [$"|Acc]);
string(<<"\\\\", T/binary>>, Acc) -> 
	string(T, [$\\|Acc]);
string(<<"\\b", T/binary>>, Acc) -> 
	string(T, [8|Acc]);
string(<<"\\t", T/binary>>, Acc) -> 
	string(T, [9|Acc]);
string(<<"\\n", T/binary>>, Acc) -> 
	string(T, [10|Acc]);
string(<<"\\f", T/binary>>, Acc) -> 
	string(T, [12|Acc]);
string(<<"\\r", T/binary>>, Acc) -> 
	string(T, [13|Acc]);
string(<<"\\u", E3, E2, E1, E0, T/binary>>, Acc) ->
	C = erlang:list_to_integer([E3, E2, E1, E0], 16),
	{Char, Rest} = if C >= 16#D800 andalso C < 16#DC00 ->
			<<"\\u", D3, D2, D1, D0, R/binary>> = T,
			D = erlang:list_to_integer([D3, D2, D1, D0], 16),
			X = (C - 16#d800) * 16#400 + (D - 16#dc00) + 16#10000,
			{X, R};
		true ->
			{C, T}
	end,
	string(Rest, [<<Char/utf8>>|Acc]);
string(<<Char, T/binary>>, Acc) -> 
	string(T, [Char|Acc]);
string(<<>>, _Acc) -> 
	erlang:error(string_not_closed).

number(<<>>, Acc) -> 
	{number(lists:reverse(Acc)), <<>>};
number(<<C, T/binary>>, Acc) when ?IS_SPACE(C) -> 
	{number(lists:reverse(Acc)), T};
number(T = <<C, _/binary>>, Acc) when ?IS_NUMBER_TERMINATOR(C) -> 
	{number(lists:reverse(Acc)), T};
number(<<$+, T/binary>>, Acc) ->
	number(T, [$+|Acc]);
number(<<$-, T/binary>>, Acc) ->
	number(T, [$-|Acc]);
number(<<$e, T/binary>>, Acc) ->
	number(T, [$e|Acc]);
number(<<$E, T/binary>>, Acc) ->
	number(T, [$e|Acc]);
number(<<".", T/binary>>, Acc) ->
	number(T, [$.|Acc]);
number(<<C, T/binary>>, Acc) when C >= $0 andalso C =< $9 ->
	number(T, [C|Acc]);
number(_, _) ->
	erlang:error(invalid_char_in_number).

number(Value) ->
	case test_integer(Value) of
		error ->
			case test_float(Value) of
				error -> erlang:error(invalid_number);
				Float -> Float
			end;
		Int -> Int
	end.

test_integer(Value) ->
	case string:to_integer(Value) of
		{Int, []} -> Int;
		_ -> error
	end.

test_float(Value) ->
	case string:to_float(Value) of
		{Float, []} -> Float;
		_ -> error
	end.
