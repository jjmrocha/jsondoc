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

-module(jsondoc_encode).

%% ====================================================================
%% API functions
%% ====================================================================
-export([encode/1]).

encode(Term) -> 
	JSon = encode(Term, []),
	iolist_to_binary(lists:reverse(JSon)).

%% ====================================================================
%% Internal functions
%% ====================================================================

encode({Term}, Acc) when is_list(Term) -> 
	from_object(Term, true, [${|Acc]);
encode(Term, Acc) when is_list(Term) -> 
	from_array(Term, true, [$[|Acc]);
encode(Term, Acc) when is_binary(Term) ->
	from_string(Term, Acc);
encode(true, Acc) ->
	[<<"true">>|Acc];
encode(false, Acc) ->
	[<<"false">>|Acc];
encode(null, Acc) ->
	[<<"null">>|Acc];
encode(Term, Acc) when is_atom(Term) ->
	from_string(Term, Acc);
encode(Term, Acc) when is_integer(Term) ->
	[integer_to_binary(Term)|Acc];
encode(Term, Acc) when is_float(Term) ->
	[from_float(Term)|Acc];
encode(_, _) ->
	erlang:error(not_valid_json).

from_object([{Key, Value}|T], First, Acc) ->
	Acc1 = object_separator(First, Acc),
	Acc2 = from_string(Key, Acc1),
	Acc3 = encode(Value, [$:|Acc2]),
	from_object(T, false, Acc3);
from_object([], _First, Acc) -> [$}|Acc].

object_separator(false, Acc) -> [$,|Acc];
object_separator(_, Acc) -> Acc.

from_array([Value|T], First, Acc) ->
	Acc1 = array_separator(First, Acc),
	Acc2 = encode(Value, Acc1),
	from_array(T, false, Acc2);
from_array([], _First, Acc) -> [$]|Acc].

array_separator(false, Acc) -> [$,|Acc];
array_separator(_, Acc) -> Acc.

from_float(Value) when Value < 0.0000000001 andalso Value > -0.0000000001 ->
	[float_to_binary(Value)];
from_float(Value) ->
	float_to_binary(Value, [{decimals, 10}, compact]).

from_string(Value, Acc) when is_binary(Value) ->
	Acc1 = safe_string(Value, [$"|Acc]),
	[$"|Acc1];
from_string(Term, Acc) when is_atom(Term) ->
	from_string(atom_to_binary(Term, utf8), Acc);
from_string(_, _) -> 
	erlang:error(invalid_string).

safe_string(<<X, T/binary>>, Acc) when X >= 32 andalso X =< 127 ->
	safe_string(T, [X|Acc]);
safe_string(<<$", T/binary>>, Acc) -> 
	safe_string(T, [<<"\\\"">>|Acc]);
safe_string(<<$\\, T/binary>>, Acc) -> 
	safe_string(T, [<<"\\\\">>|Acc]);
safe_string(<<X/utf8, T/binary>>, Acc) -> 
	C = escape(X),
	safe_string(T, [C|Acc]);
safe_string(<<>>, Acc) -> Acc.

escape(8) -> <<"\\b">>;
escape(9) -> <<"\\t">>;
escape(10) -> <<"\\n">>;
escape(12) -> <<"\\f">>;
escape(13) -> <<"\\r">>;
escape(X) -> unicode(X).

unicode(C) when C < 65536 ->
	<<D3:4, D2:4, D1:4, D0:4>> = <<C:16>>,
	E3 = hex(D3),
	E2 = hex(D2),
	E1 = hex(D1),
	E0 = hex(D0),
	<<"\\u", E3, E2, E1, E0>>;
unicode(C) ->
	X = C - 16#10000,
	<<A:10, B:10>> = <<X:20>>,
	U1 = unicode(A + 16#d800),
	U2 = unicode(B + 16#dc00),
    <<U1/binary, U2/binary>>.

hex(0) -> $0;
hex(1) -> $1;
hex(2) -> $2;
hex(3) -> $3;
hex(4) -> $4;
hex(5) -> $5;
hex(6) -> $6;
hex(7) -> $7;
hex(8) -> $8;
hex(9) -> $9;
hex(10) -> $a;
hex(11) -> $b;
hex(12) -> $c;
hex(13) -> $d;
hex(14) -> $e;
hex(15) -> $f.
