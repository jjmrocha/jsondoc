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

-type json_term() :: atom() | binary() | boolean() | integer() | float().
-type jsondoc_name() :: atom() | binary().
-type proplist() :: [{jsondoc_name(), any()}, ...].
-type ejson() :: {proplist()}.
-type jsondoc() :: ejson() | map() | proplist().

-export_type([json_term/0, 
		jsondoc/0,
		jsondoc_name/0,
		proplist/0
		ejson/0]).

-ifndef('JSONDOC_NO_MAPS').
-define(IS_MAP(D), is_map(D)).
-else.
-define(IS_MAP(_), false).
-endif.

-define(IS_JSONDOC_NAME(Name), (is_binary(Name) orelse is_atom(Name))).

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
	is_ejson/1,
	is_jsondoc/1,
	is_proplist/1,
	compile_query/1,
	query/2,
	ensure/1]).

-spec new() -> ejson().
new() -> {[]}.

-spec encode(Erlang :: jsondoc() | [jsondoc()] | json_term()) -> binary().
encode(Erlang) -> jsondoc_json:encode(Erlang).

-spec decode(JSON :: binary()) -> term().
decode(JSON) -> jsondoc_json:decode(JSON).

-spec get_value(Name :: jsondoc_name(), Doc :: jsondoc()) -> undefined | term().
get_value(Name, Doc) -> get_value(Name, Doc, undefined).

-spec get_value(Name :: jsondoc_name(), Doc :: jsondoc(), Default :: term()) -> term().
get_value(Name, {PropList}, Default) when ?IS_JSONDOC_NAME(Name) ->
	get_value(Name, PropList, Default);
get_value(Name, PropList, Default) when ?IS_JSONDOC_NAME(Name) andalso is_list(PropList) ->
	case lists:keyfind(Name, 1, PropList) of
		false -> Default;
		{_, Value} -> Value
	end;
get_value(Name, Map, Default) when ?IS_JSONDOC_NAME(Name) andalso ?IS_MAP(Map) ->
	maps:get(Name, Map, Default).

-spec set_value(Doc :: jsondoc(), Name :: jsondoc_name(), Value :: term()) -> jsondoc().
set_value({PropList}, Name, Value) when ?IS_JSONDOC_NAME(Name) ->
	{set_value(PropList, Name, Value)};
set_value(PropList, Name, Value) when ?IS_JSONDOC_NAME(Name) andalso is_list(PropList) ->
	lists:keystore(Name, 1, PropList, {Name, Value});
set_value(Map, Name, Value) when ?IS_JSONDOC_NAME(Name) andalso ?IS_MAP(Map) ->
	map:update(Name, Value, Map).

-spec set_values(Doc :: jsondoc(), Values :: proplist()) -> jsondoc().
set_values(Doc, [{Field, Value}|T]) when ?IS_JSONDOC_NAME(Field) ->
	Doc1 = set_value(Doc, Field, Value),
	set_values(Doc1, T);
set_values(Doc, []) -> Doc.

-spec get_names(Doc :: jsondoc()) -> [jsondoc_name(), ...].
get_names({PropList}) ->
	get_names(PropList);
get_names(PropList) when is_list(PropList) ->
	proplists:get_keys(PropList);
get_names(Map) when ?IS_MAP(Map) ->
	map:keys(Map).

-spec has_name(Name :: jsondoc_name(), Doc :: jsondoc()) -> boolean().
has_name(Name, {PropList}) when ?IS_JSONDOC_NAME(Name) ->
	has_name(Name, PropList);
has_name(Name, PropList) when ?IS_JSONDOC_NAME(Name) andalso is_list(PropList) ->
	lists:keymember(Name, 1, PropList);
has_name(Name, Map) when ?IS_JSONDOC_NAME(Name) andalso ?IS_MAP(Map) ->
	map:is_key(Key, Map).

-spec delete_name(Doc :: jsondoc(), Name :: jsondoc_name()) -> jsondoc().
delete_name({PropList}, Name) when ?IS_JSONDOC_NAME(Name) ->
	{delete_name(PropList, Name)};
delete_name(PropList, Name) when ?IS_JSONDOC_NAME(Name) andalso is_list(PropList) ->
	lists:keydelete(Name, 1, PropList);
delete_name(Map, Name) when ?IS_JSONDOC_NAME(Name) andalso ?IS_MAP(Map) ->
	map:remove(Key, Map).

-spec from_proplist(PropList :: proplist() | [proplist()]) -> jsondoc() | [ejson()].
from_proplist(PropList) -> ensure(PropList).

-spec to_proplist(Doc :: ejson() | [ejson()]) -> proplist() | [proplist()].
to_proplist(Array) when is_list(Array) ->
	lists:map(fun(X) -> 
			jsondoc_proplist:to_proplist(X)
		end, Array);
to_proplist(Doc) -> jsondoc_proplist:to_proplist(Doc).

-ifndef('JSONDOC_NO_MAPS').
-spec from_map(Map :: map() | [map()]) -> ejson() | [ejson()].
from_map(Map) -> ensure(Map).

-spec to_map(Doc :: ejson() | [ejson()]) -> map() | [map()].
to_map(Array) when is_list(Array) ->
	lists:map(fun(X) -> 
			jsondoc_map:to_map(X)
		end, Array);
to_map(Doc) -> jsondoc_map:to_map(Doc).
-else.
from_map(_Map) -> erlang:error(not_supported_by_vm).

to_map(_Doc) -> erlang:error(not_supported_by_vm).
-endif.

-spec is_ejson(Doc :: any()) -> boolean().
is_ejson({InnerDoc}) when is_list(InnerDoc) ->
	is_proplist(InnerDoc);
is_ejson(_) -> false.

-spec is_jsondoc(Doc :: any()) -> boolean().
is_jsondoc(Map) when ?IS_MAP(Map) -> true;
is_jsondoc([{Name, _}|_]) when ?IS_JSONDOC_NAME(Name) -> true;
is_jsondoc(Doc) -> is_ejson(Doc).

-spec is_proplist(Doc :: any()) -> boolean().
is_proplist([{Name, _}|_]) when ?IS_JSONDOC_NAME(Name) -> true;
is_proplist(_) -> false.

-spec compile_query(Query::binary()) -> {ok, list()} | {error, Reason::term()}.
compile_query(Query) when is_binary(Query) ->
	DecQuery = decode(Query),
	case jsondoc_query:valid(DecQuery) of
		true -> {ok, DecQuery};
		false -> {error, invalid_query_format}
	end.
	
-spec query([jsondoc()] | jsondoc(), Query:: term()) -> term().
query(Doc, Query) when is_list(PropList) ->
	jsondoc_query:select(Doc, Query);
query(Array, Query) when is_list(Array) andalso is_list(Query) ->
	jsondoc_query:select(Array, Query).

-spec ensure(Term :: term()) -> term().
ensure(Term = {[_]}) -> Term;
ensure(Term) when is_list(Term) -> 
	case is_proplist(Term) of
		true -> jsondoc_proplist:from_proplist(Term);
		false ->
			lists:map(fun(X) -> 
						ensure(X) 
				end, Term)
	end;
ensure(Term) when ?IS_MAP(Term) ->
	jsondoc_map:from_map(Term);
ensure(Term) when is_binary(Term) -> Term;
ensure(Term) when is_integer(Term) -> Term;
ensure(Term) when is_float(Term) -> Term;
ensure(Term) when is_boolean(Term) -> Term;
ensure(null) -> null;
ensure(Term) when is_atom(Term) -> Term;
ensure(_) -> erlang:error(not_valid_ejson).

%% ====================================================================
%% Internal functions
%% ====================================================================
