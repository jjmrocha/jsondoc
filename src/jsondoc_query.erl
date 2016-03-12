%%
%% Copyright 2013-16 Joaquim Rocha <jrocha@gmailbox.org>
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

-module(jsondoc_query).

-include("jsondoc.hrl").

-define(IS_INDEX(Index), (is_integer(Index) andalso Index >= 1)).

%% ====================================================================
%% API functions
%% ====================================================================

-export([select/2, valid/1]).

%%
%% select(Doc, Field) - Return field value
%% select(Array, Field) - Return list with field values
%% select(Array, Index) - Return element
%% select(Array, Query) - Return list on elements matching Query
%% select(JSON, Path) - Return the result of the execution of PATH
%% 
%% Where:
%% Field :: binary() | atom()
%% Doc :: jsondoc()
%% Index :: integer()
%% Array :: list()
%% Query :: {Field, Value}
%% Value :: term()
%% Path :: [PathKey, ...]
%% PathKey :: Field | Index | Query
%% JSON :: Doc | Array
%%
select(?NO_VALUE, _) -> ?NO_VALUE;
select(Value, []) -> Value;
select(JSON, [PathKey|T]) ->
	JSON1 = select(JSON, PathKey), 
	select(JSON1, T);
select(Array, Index) when is_list(Array) andalso ?IS_INDEX(Index) ->
	try lists:nth(Index, Array)
	catch _:_ -> ?NO_VALUE
	end;
select(Array, {Field, Value}) when is_list(Array) andalso ?IS_FIELD(Field) ->
	lists:filter(fun(N) -> 
			select(N, Field) == Value
		end, Array);
select(Array, Field) when is_list(Array) andalso ?IS_FIELD(Field) ->
	lists:filtermap(fun(N) -> 
			case select(N, Field) of
				?NO_VALUE -> false;
				Value -> {true, Value}
			end
		end, Array);
select(Doc, Field) when ?IS_FIELD(Field) ->
	try jsondoc:get_value(Field, Doc, ?NO_VALUE)
	catch _:_ -> ?NO_VALUE
	end;
select(_, _) -> ?NO_VALUE.

valid(Path) when is_list(Path) andalso length(Path) > 0 -> valid_path(Path);
valid(_) -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================

valid_path([H|T]) -> 
	case valid_path_key(H) of
		true -> valid_path(T);
		false -> false
	end;
valid_path([]) -> true.

valid_path_key(Field) when ?IS_FIELD(Field) -> true;
valid_path_key(Index) when ?IS_INDEX(Index) -> true;
valid_path_key({Field, _}) when ?IS_FIELD(Field) -> true;
valid_path_key(_) -> false.
