%%% @doc set implementation using maps.
%%%
%%% Copyright X4lldux 2017 &lt;x4lldux@vectron.io&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright X4lldux <x4lldux@vectron.io>
%%%
-module(mapsets).
-author("X4lldux <x4lldux@vectron.io>").

%% API
-export([ new/0
        , to_list/1
        , from_list/1
        , add_element/2
        , intersection/2
        , size/1
        ]).

-export_type([ set/0
             , set/1
             ]).

%% Define a set with maps.
-record(mapset,
        {map=#{}                 :: sets_map()      % Number of elements
        }).

-type sets_map() :: sets_map(_).
-type sets_map(E) :: #{ E => nil() }.

-type set() :: set(_).
-opaque set(Element) :: #mapset{map :: sets_map(Element)}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc returns new set
-spec new() -> set().
new() ->
  #mapset{}.

%% @doc given a set, returns a list of elements
-spec to_list(set(E)) -> list(E).
to_list(#mapset{map=Map}) ->
  maps:keys(Map).

%% @doc given a list, returns a set
-spec from_list(list(E)) -> set(E).
from_list(L) ->
  Map = maps:from_list([{E, []} || E <- L]),
  #mapset{map = Map}.

%% @doc adds an element to a set
-spec add_element(E, set(E)) -> set(E).
add_element(E, #mapset{map = Map}) ->
  #mapset{map = Map#{ E => [] }}.

%% @doc given two sets, returns their intersection
-spec intersection(set(E), set(E)) -> set(E).
intersection(#mapset{map = Map1}, #mapset{map = Map2}) ->
  {MapA, MapB} = order_by_size(Map1, Map2),
  #mapset{map = maps:with(maps:keys(MapA), MapB)}.

%% @doc given set, returns it's size
-spec size(set()) -> integer().
size(#mapset{map = Map}) ->
  map_size(Map).

-spec order_by_size(sets_map(E), sets_map(E)) -> {sets_map(E), sets_map(E)}.
order_by_size(Map1, Map2) when map_size(Map1) > map_size(Map2) -> {Map2, Map1};
order_by_size(Map1, Map2) -> {Map1, Map2}.
