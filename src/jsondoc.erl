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
	catch 
		_:{error, invalid_string} -> jiffy:encode(to_utf8(Doc))
	end.

decode(Doc) when is_binary(Doc) ->
	jiffy:decode(Doc).

get_value(Name, {PropList}) when (is_binary(Name) orelse is_atom(Name)) andalso is_list(PropList) ->
	case lists:keyfind(Name, 1, PropList) of
		false -> undefined;
		{_, Value} -> Value
	end.

set_value({PropList}, Name, Value) when is_list(PropList) andalso (is_binary(Name) orelse is_atom(Name)) ->
	{lists:keystore(Name, 1, PropList, {Name, Value})}.

has_name(Name, {PropList}) when (is_binary(Name) orelse is_atom(Name)) andalso is_list(PropList) ->
	lists:keymember(Name, 1, PropList).

delete_name({PropList}, Name) when is_list(PropList) andalso (is_binary(Name) orelse is_atom(Name)) ->
	{lists:keydelete(Name, 1, PropList)}.

string_to_utf8(Value) when is_list(Value) ->
	unicode:characters_to_binary(Value);
string_to_utf8(Value) when is_binary(Value) ->
	string_to_utf8(binary_to_list(Value)).

from_proplist(PropList) when is_list(PropList) ->
	Keys = proplists:get_keys(PropList),
	{from_proplist(Keys, PropList, [])}.

to_proplist({PropList}) when is_list(PropList) ->
	Keys = proplists:get_keys(PropList),
	to_proplist(Keys, PropList, []).

is_jsondoc(Doc) ->
	case is_tuple(Doc) of
		true -> case erlang:tuple_size(Doc) of
				1 ->
					{InnerDoc} = Doc,
					is_proplist(InnerDoc);
				_ -> false
			end;
		false -> false
	end.

is_proplist(Doc) ->
	case is_list(Doc) of
		true -> case erlang:length(Doc) of
				0 -> true;
				_ ->
					[First|_] = Doc,
					is_tuple(First) andalso erlang:tuple_size(First) == 2
			end;
		false -> false
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

to_proplist([], _PropList, OutList) -> lists:reverse(OutList);
to_proplist([Key|T], PropList, OutList) ->
	Tuple = {_, Value} = lists:keyfind(Key, 1, PropList),
	case is_jsondoc(Value) of
		true -> 
			NewPropList = to_proplist(Value),
			NewOutList = lists:keystore(Key, 1, OutList, {Key, NewPropList}),
			to_proplist(T, PropList, NewOutList);			
		false ->
			NewOutList = lists:keystore(Key, 1, OutList, Tuple),
			to_proplist(T, PropList, NewOutList)
	end.

from_proplist([], _PropList, OutList) -> lists:reverse(OutList);
from_proplist([Key|T], PropList, OutList) ->
	Tuple = {_, Value} = lists:keyfind(Key, 1, PropList),
	case is_proplist(Value) of
		true -> 
			NewDoc = from_proplist(Value),
			NewOutList = lists:keystore(Key, 1, OutList, {Key, NewDoc}),
			from_proplist(T, PropList, NewOutList);			
		false ->
			NewOutList = lists:keystore(Key, 1, OutList, Tuple),
			from_proplist(T, PropList, NewOutList)
	end.

to_utf8(Doc) when is_binary(Doc) -> string_to_utf8(Doc);
to_utf8(Doc) ->
	case is_jsondoc(Doc) of
		true ->
			{PropList} = Doc,
			Keys = proplists:get_keys(PropList),
			JSonDoc = new(),
			to_utf8(Keys, PropList, JSonDoc);
		false -> Doc
	end.

to_utf8([], _PropList, OutDoc) -> OutDoc;
to_utf8([Key|T], PropList, OutDoc) ->
	{_, Value} = lists:keyfind(Key, 1, PropList),
	NewValue = to_utf8(Value),
	NewOutDoc = set_value(OutDoc, Key, NewValue),
	to_utf8(T, PropList, NewOutDoc).