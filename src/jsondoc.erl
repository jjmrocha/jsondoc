%%
%% Copyright 2013-14 Joaquim Rocha
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
-type jsondoc() :: proplist().

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
	is_jsondoc/1,
	is_proplist/1]).

-spec new() -> jsondoc().
new() -> [].

-spec encode(Doc :: jsondoc()) -> binary().
encode(Doc) -> jsx:encode(Doc).

-spec decode(Doc :: iolist()) -> jsondoc().
decode(Doc) when is_binary(Doc) ->
	case jsx:decode(Doc) of
		[{}] -> [];
		JSonDoc -> JSonDoc
	end;		
decode(Doc) when is_list(Doc) ->
	decode(list_to_binary(Doc)).

-spec get_value(Name :: jsondoc_name(), Doc :: jsondoc()) -> undefined | any().
get_value(Name, PropList) ->
	case lists:keyfind(Name, 1, PropList) of
		false -> undefined;
		{_, Value} -> Value
	end.

-spec get_value(Name :: jsondoc_name(), Doc :: jsondoc(), Default :: any()) -> any().
get_value(Name, PropList, Default) ->
	case lists:keyfind(Name, 1, PropList) of
		false -> Default;
		{_, Value} -> Value
	end.

-spec set_value(Doc :: jsondoc(), Name :: jsondoc_name(), Value :: any()) -> jsondoc().
set_value(PropList, Name, Value) ->
	lists:keystore(Name, 1, PropList, {Name, Value}).

-spec set_values(Doc :: jsondoc(), Values :: proplist()) -> jsondoc().
set_values(Doc, [{Field, Value}|T]) ->
	Doc1 = set_value(Doc, Field, Value),
	set_values(Doc1, T);
set_values(Doc, []) -> Doc.

-spec get_names(Doc :: jsondoc()) -> [jsondoc_name(), ...].
get_names(PropList) ->
	proplists:get_keys(PropList).

-spec has_name(Name :: jsondoc_name(), Doc :: jsondoc()) -> boolean().
has_name(Name, PropList) ->
	lists:keymember(Name, 1, PropList).

-spec delete_name(Doc :: jsondoc(), Name :: jsondoc_name()) -> jsondoc().
delete_name(PropList, Name) ->
	lists:keydelete(Name, 1, PropList).

-spec is_jsondoc(Doc :: any()) -> boolean().
is_jsondoc(Doc) -> is_proplist(Doc).

-spec is_proplist(Doc :: any()) -> boolean().
is_proplist(Doc) when is_list(Doc) andalso length(Doc) > 0 ->
	[First|_] = Doc,
	is_name_value(First);
is_proplist(_) -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================

is_name_value({Name, _}) when is_binary(Name) orelse is_atom(Name) -> true;
is_name_value(_) -> false.