%%% @hidden
%%% @doc Result model.
%%%
%%% Copyright 2016 Inaka &lt;hello@inaka.net&gt;
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
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(sheldon_result).
-author("Felipe Ripoll <ferigis@gmail.com>").

%% API
-export([
          result/2
        ]).

-export_type([
               result/0
             ]).

-type result() :: ok |  #{
                           result  => [string()]
                         , bazinga => string()
                         }.

%%%===================================================================
%%% API
%%%===================================================================

-spec result([string()], sheldon_config:config()) -> result().
result([], _Config) -> ok;
result(WrongWords, _Config = #{lang := Lang}) ->
  #{
     result  => WrongWords
   , bazinga => sheldon_dictionary:get_bazinga(Lang)
   }.
