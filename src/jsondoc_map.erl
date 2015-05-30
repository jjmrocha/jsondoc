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

-module(jsondoc_map).

%% ====================================================================
%% API functions
%% ====================================================================
-export([to_map/1, from_map/1]).

to_map({PropList}) ->
	to_map(PropList, maps:new()).

from_map(Map) ->
	{from_map(maps:to_list(Map), [])}.

%% ====================================================================
%% Internal functions
%% ====================================================================

to_map([Tuple|T], OutMap) ->
	{Key, Value} = Tuple,
	NewMap = case jsondoc:is_jsondoc(Value) of
		true -> 
			NewValue = to_map(Value),
			maps:put(Key, NewValue, OutMap);	
		_ ->
			case is_list(Value) of
				true -> 
					NewValue = array_to_map(Value, []),
					maps:put(Key, NewValue, OutMap);
				_ -> maps:put(Key, Value, OutMap)
			end
	end,
	to_map(T, NewMap);
to_map([], OutMap) -> OutMap.

array_to_map([H|T], OutList) ->
	Value = case jsondoc:is_jsondoc(H) of
		true ->	to_map(H);
		_ -> 
			case is_list(H) of
				true -> array_to_map(H, []);
				_ -> H
			end
	end,
	array_to_map(T, [Value|OutList]);
array_to_map([], OutList) -> lists:reverse(OutList).

from_map([Tuple|T], OutList) ->
	{Key, Value} = Tuple,
	NewOutList = case is_map(Value) of
		true -> 
			NewDoc = from_map(Value),
			lists:keystore(Key, 1, OutList, {Key, NewDoc});		
		_ ->
			case is_list(Value) of
				true ->
					NewArray = array_from_map(Value, []),
					lists:keystore(Key, 1, OutList, {Key, NewArray});	
				_ -> lists:keystore(Key, 1, OutList, Tuple)
			end
	end,
	from_map(T, NewOutList);
from_map([], OutList) -> OutList.

array_from_map([H|T], OutList) ->
	Value = case is_map(H) of
		true ->	from_map(H);
		_ -> 
			case is_list(H) of
				true -> array_from_map(H, []);
				_ -> H
			end
	end,
	array_from_map(T, [Value|OutList]);
array_from_map([], OutList) -> lists:reverse(OutList).

