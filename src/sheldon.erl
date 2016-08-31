%%% @doc Main module for sheldon. Use this one from your own applications.
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
-module(sheldon).
-author("Felipe Ripoll <ferigis@gmail.com>").

%% API
-export([ check/1
        , check/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check(iodata()) -> sheldon_result:result().
check(Text) ->
  check(Text, sheldon_config:default()).

-spec check(iodata(), sheldon_config:config()) -> sheldon_result:result().
check(TextBin, Config1) when is_binary(TextBin) ->
  Config = sheldon_config:normalize(Config1),
  Text = binary:bin_to_list(TextBin),
  do_check(Text, Config);
check(Text, Config) ->
  TextBin = binary:list_to_bin(Text),
  check(TextBin, Config).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec do_check(string(), sheldon_config:config()) -> sheldon_result:result().
do_check(Text, Config) ->
  Words = string:tokens(Text, " "),
  MisspelledWords = check_words(Words, Config),
  sheldon_result:result(MisspelledWords, Config).

-spec check_words([string()], sheldon_config:config()) -> [string()].
check_words(Words, Config) -> check_words(Words, [], Config).

-spec check_words( [string()]
                 , [string()]
                 , sheldon_config:config()
                 ) -> [string()].
check_words([], Acc, _Config) -> Acc;
check_words([Word | Rest], Acc, Config) ->
  Normalized = sheldon_utils:normalize(Word),
  Acc1 = case correctly_spelled(Normalized, Config) of
           true  -> Acc;
           false -> [Normalized | Acc]
         end,
  check_words(Rest, Acc1, Config).

-spec correctly_spelled(string(), sheldon_config:config()) -> boolean().
correctly_spelled(Word, Config = #{lang := Lang}) ->
  case sheldon_utils:is_number(Word) orelse ignore(Word, Config) of
    false -> sheldon_dictionary:member(Word, Lang);
    true  -> true
  end.

-spec ignore(string(), sheldon_config:config()) -> boolean().
ignore(Word, _Config = #{ ignore_words    := IgnoredWords
                        , ignore_patterns := Patterns
                        }) ->
  lists:member(string:to_lower(Word), IgnoredWords)
    orelse sheldon_utils:match_in_patterns(Word, Patterns).
