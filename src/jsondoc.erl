%%
%% Copyright 2013 Joaquim Rocha
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

-module(jsondoc).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0,
	encode/1,
	decode/1,
	get_value/2,
	set_value/3,
	get_names/1,
	has_name/2,
	delete_name/2,
	string_to_utf8/1,
	from_proplist/1,
	to_proplist/1,
	is_jsondoc/1,
	is_proplist/1]).

new() -> {[]}.

encode(Doc) ->
	try jiffy:encode(Doc)
	catch _:{error, invalid_string} -> jiffy:encode(to_utf8(Doc))
	end.

decode(Doc) when is_binary(Doc) ->
	jiffy:decode(Doc);
decode(Doc) when is_list(Doc) ->
	jiffy:decode(Doc).

get_value(Name, {PropList}) when (is_binary(Name) orelse is_atom(Name)) andalso is_list(PropList) ->
	case lists:keyfind(Name, 1, PropList) of
		false -> undefined;
		{_, Value} -> Value
	end.

set_value({PropList}, Name, Value) when is_list(PropList) andalso (is_binary(Name) orelse is_atom(Name)) ->
	{lists:keystore(Name, 1, PropList, {Name, Value})}.

get_names({PropList}) when is_list(PropList) ->
	proplists:get_keys(PropList).

has_name(Name, {PropList}) when (is_binary(Name) orelse is_atom(Name)) andalso is_list(PropList) ->
	lists:keymember(Name, 1, PropList).

delete_name({PropList}, Name) when is_list(PropList) andalso (is_binary(Name) orelse is_atom(Name)) ->
	{lists:keydelete(Name, 1, PropList)}.

string_to_utf8(Value) when is_list(Value) ->
	unicode:characters_to_binary(Value);
string_to_utf8(Value) when is_binary(Value) ->
	string_to_utf8(binary_to_list(Value)).

from_proplist(PropList) when is_list(PropList) ->
	{from_proplist(PropList, [])}.

to_proplist({PropList}) when is_list(PropList) ->
	to_proplist(PropList, []).

is_jsondoc(Doc) when is_tuple(Doc) andalso tuple_size(Doc) == 1 ->
	{InnerDoc} = Doc,
	is_proplist(InnerDoc);
is_jsondoc(_) -> false.

is_proplist(Doc) when is_list(Doc) andalso length(Doc) > 0 ->
	[First|_] = Doc,
	is_name_value(First);
is_proplist(_) -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================

is_name_value({Name, _}) when is_binary(Name) orelse is_atom(Name) -> true;
is_name_value(_) -> false.

to_proplist([], OutList) -> OutList;
to_proplist([Tuple|T], OutList) ->
	{Key, Value} = Tuple,
	NewOutList = case is_jsondoc(Value) of
		true -> 
			NewPropList = to_proplist(Value),
			lists:keystore(Key, 1, OutList, {Key, NewPropList});	
		_ ->
			case is_list(Value) of
				true -> 
					NewArray = array_to_proplist(Value, []),
					lists:keystore(Key, 1, OutList, {Key, NewArray});
				_ -> lists:keystore(Key, 1, OutList, Tuple)
			end
	end,
	to_proplist(T, NewOutList).

array_to_proplist([], OutList) -> lists:reverse(OutList);
array_to_proplist([H|T], OutList) ->
	Value = case is_jsondoc(H) of
		true ->	to_proplist(H);
		_ -> 
			case is_list(H) of
				true -> array_to_proplist(H, []);
				_ -> H
			end
	end,
	array_to_proplist(T, [Value|OutList]).

from_proplist([], OutList) -> OutList;
from_proplist([Tuple|T], OutList) ->
	{Key, Value} = Tuple,
	NewOutList = case is_proplist(Value) of
		true -> 
			NewDoc = from_proplist(Value),
			lists:keystore(Key, 1, OutList, {Key, NewDoc});		
		_ ->
			case is_list(Value) of
				true ->
					NewArray = array_from_proplist(Value, []),
					lists:keystore(Key, 1, OutList, {Key, NewArray});	
				_ -> lists:keystore(Key, 1, OutList, Tuple)
			end
	end,
	from_proplist(T, NewOutList).

array_from_proplist([], OutList) -> lists:reverse(OutList);
array_from_proplist([H|T], OutList) ->
	Value = case is_proplist(H) of
		true ->	from_proplist(H);
		_ -> 
			case is_list(H) of
				true -> array_from_proplist(H, []);
				_ -> H
			end
	end,
	array_from_proplist(T, [Value|OutList]).

to_utf8(Doc) when is_binary(Doc) -> string_to_utf8(Doc);
to_utf8(Doc) when is_list(Doc) -> array_to_utf8(Doc, []);
to_utf8(Doc) ->
	case is_jsondoc(Doc) of
		true ->
			{PropList} = Doc,
			JSonDoc = new(),
			to_utf8(PropList, JSonDoc);
		false -> Doc
	end.

to_utf8([], OutDoc) -> OutDoc;
to_utf8([Tuple|T], OutDoc) ->
	{Key, Value} = Tuple,
	NewValue = to_utf8(Value),
	NewOutDoc = set_value(OutDoc, Key, NewValue),
	to_utf8(T, NewOutDoc).

array_to_utf8([], OutList) -> lists:reverse(OutList);
array_to_utf8([H|T], OutList) when is_binary(H) ->
	Utf8 = string_to_utf8(H),
	array_to_utf8(T, [Utf8|OutList]);
array_to_utf8([H|T], OutList) -> array_to_utf8(T, [H|OutList]).