%%% @doc Utils module for sheldon.
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
-module(sheldon_utils).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% API
-export([ normalize/1
        , is_number/1
        , match_in_patterns/2
        ]).

-compile({no_auto_import, [is_number/1]}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc normalizes the iodata() escaping some characters and converting
%%      them to string().
-spec normalize(iodata()) -> string().
normalize(Word) ->
  Escaped = escape_chars(Word),
  binary_to_list(Escaped).

%% @doc checks if iodata() is a number
-spec is_number(iodata()) -> boolean().
is_number(Word) ->
  re:run(Word, "^[0-9]*$") =/= nomatch.

%% @doc checks if some string() matches in one of the patterns given as
%%      a parameter
-spec match_in_patterns(string(), [string()]) -> boolean().
match_in_patterns(Word, Patterns) ->
  MatchTuples = [{Word, Pattern} || Pattern <- Patterns],
  lists:foldl(fun match/2, false, MatchTuples).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec escape_chars(iodata()) -> binary().
escape_chars(Word) ->
  Word1 = escape_patterns(Word, prefixes(), prefixes()),
  Word2 = escape_patterns(Word1, sufixes(), sufixes()),
  case escape_patterns(Word2, special_chars(), special_chars()) of
    EscapedWord when is_list(EscapedWord) ->
      list_to_binary(EscapedWord);
    EscapedWord ->
      EscapedWord
  end.

-spec match({string(), string()}, boolean()) -> boolean().
match(_, true) -> true;
match({Word, Pattern}, false) ->
  re:run(Word, Pattern) =/= nomatch.

-spec escape_patterns(iodata(), [iodata()], [iodata()]) -> iodata().
escape_patterns(Word, [], _) ->
  Word;
escape_patterns(Word, [Pattern | Patterns], OriginalPatterns) ->
  case re:replace(Word, Pattern, "", [global]) of
    Word  -> escape_patterns(Word, Patterns, OriginalPatterns);
    Word1 -> escape_patterns(Word1, OriginalPatterns, OriginalPatterns)
  end.

-spec prefixes() -> [sheldon_config:regex()].
prefixes() ->
  [ "^[(]"
  , "^\""
  , "^\'"
  , "^[[]"
  , "^{"
  , "^`"
  ].

-spec sufixes() -> [sheldon_config:regex()].
sufixes() ->
  [ "[.]$"
  , ",$"
  , ":$"
  , ";$"
  , "[?]$"
  , "[)]$"
  , "!$"
  , "]$"
  , "}$"
  , "'s$"
  , "\"$"
  , "\'$"
  , "`$"
  , "\n"
  ].

-spec special_chars() -> [sheldon_config:regex()].
special_chars() ->
  [ "^&$"
  , "^>$"
  , "^<$"
  ].
