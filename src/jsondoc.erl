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

-ifndef('JSONDOC_NO_MAPS').
-define(IS_MAP(D), is_map(D)).
-else.
-define(IS_MAP(_), (1 =:= 0)).
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
	from_map/1,
	to_map/1,		 
	is_jsondoc/1,
	is_proplist/1,
	query/1,
	query/2,
	ensure/1]).

-spec new() -> jsondoc().
new() -> {[]}.

-spec encode(Doc :: term()) -> binary().
encode(Doc) ->
	jsondoc_json:encode(Doc).

-spec decode(Doc :: binary()) -> term().
decode(Doc) ->
	jsondoc_json:decode(Doc).

-spec get_value(Name :: jsondoc_name(), Doc :: jsondoc()) -> undefined | any().
get_value(Name, Doc) ->
	get_value(Name, Doc, undefined).

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
	jsondoc_proplist:from_proplist(PropList).

-spec to_proplist(Doc :: jsondoc()) -> proplist().
to_proplist(Doc) ->
	jsondoc_proplist:to_proplist(Doc).

-ifndef('JSONDOC_NO_MAPS').
-spec from_map(Map :: map()) -> jsondoc().
from_map(Map) ->
	jsondoc_map:from_map(Map).

-spec to_map(Doc :: jsondoc()) -> map().
to_map(Doc) ->
	jsondoc_map:to_map(Doc).
-else.
from_map(Map) ->
	erlang:error(not_supported_by_vm).

to_map(Doc) ->
	erlang:error(not_supported_by_vm).
-endif.

-spec is_jsondoc(Doc :: any()) -> boolean().
is_jsondoc({InnerDoc}) when is_list(InnerDoc) ->
	is_proplist(InnerDoc);
is_jsondoc(_) -> false.

-spec is_proplist(Doc :: any()) -> boolean().
is_proplist([{Name, _}|_]) when is_binary(Name) orelse is_atom(Name) -> true;
is_proplist(_) -> false.

-spec query(Query::binary()) -> {ok, list()} | {error, Reason::term()}.
query(Query) when is_binary(Query) ->
	DecQuery = decode(Query),
	case jsondoc_query:valid(DecQuery) of
		true -> {ok, DecQuery};
		false -> {error, invalid_query_format}
	end.
	
-spec query(list() | jsondoc(), Query:: term()) -> term().
query(Doc = {PropList}, Query) when is_list(PropList) ->
	jsondoc_query:select(Doc, Query);
query(Array, Query) when is_list(Array) ->
	jsondoc_query:select(Array, Query).

-spec ensure(Term :: term()) -> term().
ensure(Term = {[_]}) -> Term;
ensure(Term) when is_list(Term) -> 
	case is_proplist(Term) of
		true -> from_proplist(Term);
		false ->
			lists:map(fun(X) -> 
						ensure(X) 
				end, Term)
	end;
ensure(Term) when ?IS_MAP(Term) ->
	from_map(Term);
ensure(Term) -> Term.

%% ====================================================================
%% Internal functions
%% ====================================================================
