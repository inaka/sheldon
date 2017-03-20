%%% @doc Utils module for sheldon.
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
  Word1 = apply_rules(Word),
  binary_to_list(Word1).

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

-spec match({string(), string()}, boolean()) -> boolean().
match(_, true) -> true;
match({Word, Pattern}, false) ->
  re:run(Word, Pattern) =/= nomatch.

-spec apply_rules(iodata()) -> binary().
apply_rules(Word) ->
  Word1 = escape_md_symbols(Word),
  Word2 = escape_prefixes(Word1),
  Word3 = escape_suffixes(Word2),
  escape_word(Word3).

-spec escape_word(iodata()) -> binary().
escape_word(Word) ->
  EscapedWords = sheldon_basic_check:escaped_words(),
  escape_word(Word, EscapedWords).

-spec escape_word(iodata(), list()) -> binary().
escape_word(Word, []) ->
  Word;
escape_word(Word, [{pattern, Pattern} | RestEscapedWords]) ->
  case re:run(Word, Pattern) of
    nomatch -> escape_word(Word, RestEscapedWords);
    _ -> <<>>
  end;
escape_word(Word, [Word | _]) ->
  <<>>;
escape_word(Word, [_ | RestEscapedWords]) ->
  escape_word(Word, RestEscapedWords).

-spec escape_prefixes(iodata()) -> binary().
escape_prefixes(Word) ->
  Prefixes = sheldon_basic_check:prefixes(),
  escape_prefixes(Word, Prefixes, Prefixes).

-spec escape_prefixes(iodata(), [binary()], [binary()]) -> binary().
escape_prefixes(Word, [], _) ->
  Word;
escape_prefixes(Word, [Prefix | RestPrefixes], Prefixes) ->
  PrefixSize = byte_size(Prefix),
  case Word of
    <<Prefix:PrefixSize/binary, RestWord/binary>> ->
      % There is a match, we have to check again with all the prefixes
      escape_prefixes(RestWord, Prefixes, Prefixes);
    _ ->
      escape_prefixes(Word, RestPrefixes, Prefixes)
  end.

-spec escape_suffixes(iodata()) -> binary().
escape_suffixes(Word) ->
  Suffixes = sheldon_basic_check:suffixes(),
  escape_suffixes(Word, Suffixes, Suffixes).

-spec escape_suffixes(iodata(), [binary()], [binary()]) -> binary().
escape_suffixes(Word, [], _) ->
  Word;
escape_suffixes(Word, [Suffix | RestSuffixes], Suffixes) ->
  SuffixSize = byte_size(Suffix),
  NewWordSize = byte_size(Word) - SuffixSize,
  case Word of
    <<RestWord:NewWordSize/binary, Suffix:SuffixSize/binary>> ->
      % There is a match, we have to check again with all the suffixes
      escape_suffixes(RestWord, Suffixes, Suffixes);
    _ ->
      escape_suffixes(Word, RestSuffixes, Suffixes)
  end.

-spec escape_md_symbols(iodata()) -> binary().
escape_md_symbols(Word) ->
  [Word1 | _] = re:split(Word, <<"[]][(]">>),
  Word1.
