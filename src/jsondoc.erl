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

-type jsondoc_name() :: atom() | binary().
-type proplist() :: [{jsondoc_name(), any()}, ...].
-type jsondoc() :: {proplist()}.

-export_type([jsondoc/0,
			  jsondoc_name/0,
			  proplist/0]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0,
	encode/1,
	decode/1,
	get_value/2,
	get_value/3,
	set_value/3,
	set_values/2,
	get_names/1,
	has_name/2,
	delete_name/2,
	string_to_utf8/1,
	string_from_utf8/1,
	from_proplist/1,
	to_proplist/1,
	is_jsondoc/1,
	is_proplist/1,
	to_utf8/1,
	from_utf8/1]).

-spec new() -> jsondoc().
new() -> {[]}.

-spec encode(Doc :: jsondoc()) -> binary().
encode(Doc) ->
	try jiffy:encode(Doc)
	catch _:{error, invalid_string} -> jiffy:encode(to_utf8(Doc))
	end.

-spec decode(Doc :: iolist()) -> jsondoc().
decode(Doc) when is_binary(Doc) ->
	jiffy:decode(Doc);
decode(Doc) when is_list(Doc) ->
	jiffy:decode(Doc).

-spec get_value(Name :: jsondoc_name(), Doc :: jsondoc()) -> undefined | any().
get_value(Name, {PropList}) ->
	case lists:keyfind(Name, 1, PropList) of
		false -> undefined;
		{_, Value} -> Value
	end.

-spec get_value(Name :: jsondoc_name(), Doc :: jsondoc(), Default :: any()) -> any().
get_value(Name, {PropList}, Default) ->
	case lists:keyfind(Name, 1, PropList) of
		false -> Default;
		{_, Value} -> Value
	end.

-spec set_value(Doc :: jsondoc(), Name :: jsondoc_name(), Value :: any()) -> jsondoc().
set_value({PropList}, Name, Value) ->
	{lists:keystore(Name, 1, PropList, {Name, Value})}.

-spec set_values(Doc :: jsondoc(), Values :: proplist()) -> jsondoc().
set_values(Doc, [{Field, Value}|T]) ->
	Doc1 = set_value(Doc, Field, Value),
	set_values(Doc1, T);
set_values(Doc, []) -> Doc.

-spec get_names(Doc :: jsondoc()) -> [jsondoc_name(), ...].
get_names({PropList}) ->
	proplists:get_keys(PropList).

-spec has_name(Name :: jsondoc_name(), Doc :: jsondoc()) -> boolean().
has_name(Name, {PropList}) ->
	lists:keymember(Name, 1, PropList).

-spec delete_name(Doc :: jsondoc(), Name :: jsondoc_name()) -> jsondoc().
delete_name({PropList}, Name) ->
	{lists:keydelete(Name, 1, PropList)}.

-spec string_to_utf8(Value :: iolist()) -> binary().
string_to_utf8(Value) when is_list(Value) ->
	unicode:characters_to_binary(Value);
string_to_utf8(Value) when is_binary(Value) ->
	string_to_utf8(binary_to_list(Value)).

-spec string_from_utf8(Value :: iolist()) -> binary().
string_from_utf8(Value) when is_binary(Value) ->
	UTF8 = unicode:characters_to_list(Value),
	list_to_binary(UTF8);
string_from_utf8(Value) when is_list(Value) ->
	string_from_utf8(list_to_binary(Value)).

-spec from_proplist(PropList :: proplist()) -> jsondoc().
from_proplist(PropList) ->
	{from_proplist(PropList, [])}.

-spec to_proplist(Doc :: jsondoc()) -> proplist().
to_proplist({PropList}) ->
	to_proplist(PropList, []).

-spec is_jsondoc(Doc :: any()) -> boolean().
is_jsondoc(Doc) when is_tuple(Doc) andalso tuple_size(Doc) == 1 ->
	{InnerDoc} = Doc,
	is_proplist(InnerDoc);
is_jsondoc(_) -> false.

-spec is_proplist(Doc :: any()) -> boolean().
is_proplist(Doc) when is_list(Doc) andalso length(Doc) > 0 ->
	[First|_] = Doc,
	is_name_value(First);
is_proplist(_) -> false.

-spec to_utf8(Value :: any()) -> any().
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

-spec from_utf8(Value :: any()) -> any().
from_utf8(Doc) when is_binary(Doc) -> string_from_utf8(Doc);
from_utf8(Doc) when is_list(Doc) -> array_from_utf8(Doc, []);
from_utf8(Doc) ->
	case is_jsondoc(Doc) of
		true ->
			{PropList} = Doc,
			JSonDoc = new(),
			from_utf8(PropList, JSonDoc);
		false -> Doc
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

is_name_value({Name, _}) when is_binary(Name) orelse is_atom(Name) -> true;
is_name_value(_) -> false.

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
	to_proplist(T, NewOutList);
to_proplist([], OutList) -> OutList.

array_to_proplist([H|T], OutList) ->
	Value = case is_jsondoc(H) of
		true ->	to_proplist(H);
		_ -> 
			case is_list(H) of
				true -> array_to_proplist(H, []);
				_ -> H
			end
	end,
	array_to_proplist(T, [Value|OutList]);
array_to_proplist([], OutList) -> lists:reverse(OutList).

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
	from_proplist(T, NewOutList);
from_proplist([], OutList) -> OutList.

array_from_proplist([H|T], OutList) ->
	Value = case is_proplist(H) of
		true ->	from_proplist(H);
		_ -> 
			case is_list(H) of
				true -> array_from_proplist(H, []);
				_ -> H
			end
	end,
	array_from_proplist(T, [Value|OutList]);
array_from_proplist([], OutList) -> lists:reverse(OutList).

to_utf8([Tuple|T], OutDoc) ->
	{Key, Value} = Tuple,
	NewValue = to_utf8(Value),
	NewOutDoc = set_value(OutDoc, Key, NewValue),
	to_utf8(T, NewOutDoc);
to_utf8([], OutDoc) -> OutDoc.

array_to_utf8([H|T], OutList) when is_binary(H) ->
	Utf8 = string_to_utf8(H),
	array_to_utf8(T, [Utf8|OutList]);
array_to_utf8([H|T], OutList) -> array_to_utf8(T, [H|OutList]);
array_to_utf8([], OutList) -> lists:reverse(OutList).

from_utf8([Tuple|T], OutDoc) ->
	{Key, Value} = Tuple,
	NewValue = from_utf8(Value),
	NewOutDoc = set_value(OutDoc, Key, NewValue),
	from_utf8(T, NewOutDoc);
from_utf8([], OutDoc) -> OutDoc.

array_from_utf8([H|T], OutList) when is_binary(H) ->
	Utf8 = string_from_utf8(H),
	array_from_utf8(T, [Utf8|OutList]);
array_from_utf8([H|T], OutList) -> array_from_utf8(T, [H|OutList]);
array_from_utf8([], OutList) -> lists:reverse(OutList).