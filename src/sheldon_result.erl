%%% @doc Result model.
%%%
%%% Copyright Erlang Solutions Ltd. 2017 &lt;hello@inaka.net&gt;
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
%%% @copyright Erlang Solutions Ltd. <hello@inaka.net>
%%%
-module(sheldon_result).

-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% API
-export([result/2]).

-export_type([result/0, misspelled_word/0, line_number/0]).

-type line_number() :: pos_integer().
-type misspelled_word() ::
    #{line_number := line_number(),
      word := string(),
      candidates := [string()]}.
-type result() :: ok | #{misspelled_words := [misspelled_word()], bazinga := string()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns a result() type given some misspelled words.
-spec result([misspelled_word()], sheldon_config:config()) -> result().
result([], _Config) ->
    ok;
result(MisspelledWords, _Config = #{lang := Lang}) ->
    #{misspelled_words => MisspelledWords, bazinga => sheldon_dictionary:get_bazinga(Lang)}.
