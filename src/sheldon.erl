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
-export([ start/0
        , check/1
        , check/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(sheldon),
  ok.

-spec check(iodata()) -> sheldon_result:result().
check(Text) ->
  check(Text, sheldon_config:default()).

-spec check(iodata(), sheldon_config:config()) -> sheldon_result:result().
check(Text, Config1) ->
  Config = sheldon_config:normalize(Config1),
  do_check(Text, Config).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec do_check(iodata(), sheldon_config:config()) -> sheldon_result:result().
do_check(Text, Config) ->
  Lines = re:split(Text, "\n"),
  Misspelled = check_lines(Lines, 1, [], Config),
  sheldon_result:result(Misspelled, Config).

-spec check_lines( [iodata()]
                 , sheldon_result:line_number()
                 , [sheldon_result:misspelled_word()]
                 , sheldon_config:config()
                 ) -> [sheldon_result:misspelled_word()].
check_lines([], _LineNumber, MisspelledWords, _Config) ->
  MisspelledWords;
check_lines([Line | RestFile], LineNumber, MisspelledWords, Config) ->
  Words = re:split(Line, " "),
  case check_words(Words, [], Config) of
    {ok, Result} ->
      MisspelledWords2 =
        [#{ line_number => LineNumber, word => Word } || Word <- Result],
      MisspelledWords3 = lists:flatten([MisspelledWords2 | MisspelledWords]),
      check_lines(RestFile, LineNumber + 1, MisspelledWords3, Config);
    {in_block, CloseBlock} ->
      case find_block_closing(RestFile, LineNumber + 1, CloseBlock, Config) of
        eof                      -> MisspelledWords;
        {RestFile2, LineNumber2} ->
          check_lines(RestFile2, LineNumber2, MisspelledWords, Config)
      end
  end.

-spec check_words( [iodata()]
                 , [string()]
                 , sheldon_config:config()
                 ) -> {in_block, sheldon_config:regex()} | {ok, [string()]}.
check_words([], Misspelled, _Config) -> {ok, Misspelled};
check_words([Word | RestLine], Misspelled, Config) ->
  #{ ignore_blocks := IgnoreBlocks } = Config,
  case block_opening(Word, IgnoreBlocks) of
    no_block ->
      Misspelled1 = check_word(Word, Misspelled, Config),
      check_words(RestLine, Misspelled1, Config);
    {open, CloseBlock} ->
      case find_close_pattern(RestLine, CloseBlock) of
        {closed, RestLine2} -> check_words(RestLine2, Misspelled, Config);
        no_closed -> {in_block, CloseBlock}
      end
  end.

-spec check_word(iodata(), [string()], sheldon_config:config()) -> [string()].
check_word(Word, Misspelled, Config) ->
  Normalized = sheldon_utils:normalize(Word),
  case correctly_spelled(Normalized, Config) of
    true  -> Misspelled;
    false -> [Normalized | Misspelled]
  end.

-spec block_opening( iodata()
                   , [sheldon_config:ignore_block()]
                   ) -> no_block | {open, sheldon_config:regex()}.
block_opening(_, []) -> no_block;
block_opening(Word, [#{ open := OpenBlock, close := CloseBlock } | Rest]) ->
  case re:run(Word, OpenBlock) of
    nomatch -> block_opening(Word, Rest);
    _ -> {open, CloseBlock}
  end.

-spec find_block_closing( [iodata()]
                        , sheldon_result:line_number()
                        , sheldon_result:regex()
                        , sheldon_config:config()
                        ) -> eof | {[iodata()], sheldon_result:line_number()}.
find_block_closing([], _LineNumber, _CloseBlock, _Config) ->
  % file has ended without close pattern
  eof;
find_block_closing([Line | RestFile], LineNumber, CloseBlock, Config) ->
  Words = re:split(Line, " "),
  case find_close_pattern(Words, CloseBlock) of
    {closed, RestWords} ->
      % create a line with the rest of the words
      RestLine = [[L, " "] || L <- RestWords],
      RestFile2 = [RestLine | RestFile],
      {RestFile2, LineNumber};
    no_closed ->
      find_block_closing(RestFile, LineNumber + 1, CloseBlock, Config)
  end.

-spec find_close_pattern( [iodata()]
                        , sheldon_config:regex()
                        ) -> no_closed | {closed, [iodata()]}.
find_close_pattern([], _CloseBlock) ->
  % The line has ended without close pattern
  no_closed;
find_close_pattern([Word | RestWords] = _Line, CloseBlock) ->
  case re:run(Word, CloseBlock) of
    nomatch -> find_close_pattern(RestWords, CloseBlock);
    _       -> {closed, RestWords}
  end.

-spec correctly_spelled(string(), sheldon_config:config()) -> boolean().
correctly_spelled(Word, Config = #{ lang := Lang }) ->
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
