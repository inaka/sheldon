%%% @doc Main module for sheldon. Use this one from your own applications.
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
-module(sheldon).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% API
-export([ start/0
        , check/1
        , check/2
        ]).

%% Types
-type line() :: {pos_integer(), iodata()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(sheldon),
  ok.

%% @doc Checks the spelling of the iodata() received as a parameter.
%% @equiv check(Text, sheldon_config:default())
-spec check(iodata()) -> sheldon_result:result().
check(Text) ->
  check(Text, sheldon_config:default()).

%% @doc Checks the spelling of the iodata() received as a parameter.
%%      It also accepts Config parameter
-spec check(iodata(), sheldon_config:config()) -> sheldon_result:result().
check(Text, Config1) ->
  Config = sheldon_config:normalize(Config1),
  do_check(Text, Config).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec do_check(iodata(), sheldon_config:config()) -> sheldon_result:result().
do_check(Text, #{ ignore_blocks := IgnoreBlocks } = Config) ->
  Lines = re:split(Text, "\n"),

  % Create lines in format {lineNumber, Line}
  Lines2 = lists:zip(lists:seq(1, length(Lines)), Lines),

  Lines3 = escape_blocks(Lines2, IgnoreBlocks),
  Misspelled = check_lines(Lines3, [], Config),

  FullMisspelled = add_candidates(Misspelled, Config),

  sheldon_result:result(FullMisspelled, Config).

-spec add_candidates([sheldon_result:misspelled_word()], sheldon_config:config()) ->
  [sheldon_result:misspelled_word()].
add_candidates(MisspelledWords, #{lang := Lang}) ->
  sheldon_suggestions_server:suggestions(MisspelledWords, Lang).

-spec escape_blocks([line()], [sheldon_config:ignore_block()]) -> [line()].
escape_blocks(Lines, IgnoreBlocks) ->
  escape_blocks(Lines, IgnoreBlocks, []).

-spec escape_blocks( [line()] | {in_block, sheldon_config:regex(), [line()]}
                   , [sheldon_config:ignore_block()]
                   , [line()]
                   ) -> [line()].
escape_blocks([], _, Lines) ->
  Lines;
escape_blocks([{_, <<>>} | Rest], IgnoreBlocks, Lines) ->
  escape_blocks(Rest, IgnoreBlocks, Lines);
escape_blocks([Line | Rest], IgnoreBlocks, Lines) ->
  case clean_line(Line, IgnoreBlocks) of
    {in_block, CloseBlock, CleanLine} ->
      escape_blocks({in_block, CloseBlock, Rest}, IgnoreBlocks, [CleanLine | Lines]);
    CleanLine ->
      escape_blocks(Rest, IgnoreBlocks, [CleanLine | Lines])
  end;
escape_blocks({in_block, _CloseBlock, []}, _, Lines) ->
  Lines;
escape_blocks({in_block, CloseBlock, [{LineNumber, Line} | Rest]}, IgnoreBlocks, Lines) ->
  Words = re:split(Line, " "),
  case find_close_pattern(Words, CloseBlock) of
    {closed, RestLine2} ->
      escape_blocks([{LineNumber, build_line(RestLine2)} | Rest], IgnoreBlocks, Lines);
    no_closed ->
      escape_blocks({in_block, CloseBlock, Rest}, IgnoreBlocks, Lines)
  end.

-spec clean_line(line(), [sheldon_config:ignore_blocks()]) ->
  line() | {in_block, sheldon_config:regex(), line()}.
clean_line({LineNumber, Line}, IgnoreBlocks) ->
  Words = re:split(Line, " "),
  case find_block(Words, IgnoreBlocks, []) of
    {in_block, CloseBlock, CleanLine} ->
      {in_block, CloseBlock, {LineNumber, build_line(CleanLine)}};
    CleanLine ->
      {LineNumber, build_line(CleanLine)}
  end.

-spec find_block([binary()], sheldon_config:ignore_blocks(), [binary()]) ->
  [binary()] | {in_block, sheldon_config:regex(), [binary()]}.
find_block([], _, CleanLine) ->
  CleanLine;
find_block([Word | RestLine], IgnoreBlocks, CleanLine) ->
  case block_opening(Word, IgnoreBlocks) of
    no_block ->
      find_block(RestLine, IgnoreBlocks, [Word | CleanLine]);
    {open, CloseBlock} ->
      case find_close_pattern(RestLine, CloseBlock) of
        {closed, RestLine2} -> find_block(RestLine2, IgnoreBlocks, CleanLine);
        no_closed -> {in_block, CloseBlock, CleanLine}
      end
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

-spec build_line([binary()]) -> binary().
build_line([]) ->
  <<>>;
build_line(Words) ->
  Words1 = lists:reverse(Words),
  <<_, Line/binary>> = << <<$\s, Word/binary>> || Word <- Words1 >>,
  Line.


-spec check_lines([line()], [sheldon_result:misspelled_word()], sheldon_config:config()) ->
  [sheldon_result:misspelled_word()].
check_lines([], MisspelledWords, _Config) ->
  MisspelledWords;
check_lines( [{LineNumber, Line} | RestFile]
           , MisspelledWords
           , Config = #{ adapters := Adapters }
           ) ->
  Line1 = sheldon_config:apply_adapters(Line, Adapters),
  WordsOrdered = re:split(Line1, " "),
  Words = lists:reverse(WordsOrdered),
  {ok, Result} = check_words(Words, [], Config),
  MisspelledWords2 =
    [#{ line_number => LineNumber
      , word => Word
      , candidates => []
      } || Word <- Result],
  MisspelledWords3 = lists:append(MisspelledWords2, MisspelledWords),
  check_lines(RestFile, MisspelledWords3, Config).

-spec check_words([binary()], [string()], sheldon_config:config()) -> {ok, [string()]}.
check_words([], Misspelled, _Config) -> {ok, Misspelled};
check_words([Word | RestLine], Misspelled, Config) ->
  Misspelled1 = check_word(Word, Misspelled, Config),
  check_words(RestLine, Misspelled1, Config).

-spec check_word(iodata(), [string()], sheldon_config:config()) -> [string()].
check_word(Word, Misspelled, Config) ->
  Normalized = sheldon_utils:normalize(Word),
  case correctly_spelled(Normalized, Config) of
    true  -> Misspelled;
    false -> [Normalized | Misspelled]
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
