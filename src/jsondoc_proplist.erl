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

-module(jsondoc_proplist).

%% ====================================================================
%% API functions
%% ====================================================================
-export([to_proplist/1, from_proplist/1]).

to_proplist({PropList}) ->
	to_proplist(PropList, []).

from_proplist(PropList) ->
	{from_proplist(PropList, [])}.

%% ====================================================================
%% Internal functions
%% ====================================================================

to_proplist([Tuple|T], OutList) ->
	{Key, Value} = Tuple,
	NewOutList = case jsondoc:is_jsondoc(Value) of
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
	Value = case jsondoc:is_jsondoc(H) of
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
	NewOutList = case jsondoc:is_proplist(Value) of
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
	Value = case jsondoc:is_proplist(H) of
		true ->	from_proplist(H);
		_ -> 
			case is_list(H) of
				true -> array_from_proplist(H, []);
				_ -> H
			end
	end,
	array_from_proplist(T, [Value|OutList]);
array_from_proplist([], OutList) -> lists:reverse(OutList).

