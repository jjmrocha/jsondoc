%%
%% Copyright 2013-15 Joaquim Rocha <jrocha@gmailbox.org>
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

-ifndef('WITH_JIFFY').
-define(JSON_ENCODE(D), jsondoc_json:encode(D)).
-define(JSON_DECODE(D), jsondoc_json:decode(D)).
-else.
-define(JSON_ENCODE(D), jiffy:encode(D, [uescape])).
-define(JSON_DECODE(D), jiffy:decode(D)).
-endif.

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
	from_proplist/1,
	to_proplist/1,
	is_jsondoc/1,
	is_proplist/1]).

-spec new() -> jsondoc().
new() -> {[]}.

-spec encode(Doc :: jsondoc()) -> binary().
encode(Doc) ->
	?JSON_ENCODE(Doc).

-spec decode(Doc :: binary()) -> jsondoc().
decode(Doc) ->
	?JSON_DECODE(Doc).

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

-spec from_proplist(PropList :: proplist()) -> jsondoc().
from_proplist(PropList) ->
	{from_proplist(PropList, [])}.

-spec to_proplist(Doc :: jsondoc()) -> proplist().
to_proplist({PropList}) ->
	to_proplist(PropList, []).

-spec is_jsondoc(Doc :: any()) -> boolean().
is_jsondoc({InnerDoc}) when is_list(InnerDoc) ->
	is_proplist(InnerDoc);
is_jsondoc(_) -> false.

-spec is_proplist(Doc :: any()) -> boolean().
is_proplist([{Name, _}|_]) when is_binary(Name) orelse is_atom(Name) -> true;
is_proplist(_) -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================

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