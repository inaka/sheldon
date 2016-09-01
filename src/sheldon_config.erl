%%% @hidden
%%% @doc sheldon's module for config handling.
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
-module(sheldon_config).
-author("Felipe Ripoll <ferigis@gmail.com>").

%% API
-export([ default/0
        , normalize/1
        ]).

-export_type([ config/0
             ]).

-type regex()  :: iodata().
-type ignore_block() ::
  #{ open  := regex()
   , close := regex()
   }.
-type config() ::
  #{ lang            => sheldon_dictionary:language()
   , ignore_words    => [string()]
   , ignore_patterns => [string()]
   , ignore_blocks   => [ignore_block()]
   }.

%%%===================================================================
%%% API
%%%===================================================================

-spec default() -> config().
default() ->
  #{ lang            => default_lang()
   , ignore_words    => default_ignore_words()
   , ignore_patterns => default_ignore_patterns()
   , ignore_blocks   => default_ignore_blocks()
   }.

-spec normalize(config()) -> config().
normalize(Config) ->
  #{ lang            => normalize_lang(Config)
   , ignore_words    => normalize_ignore_words(Config)
   , ignore_patterns => normalize_ignore_patterns(Config)
   , ignore_blocks   => normalize_ignore_blocks(Config)
   }.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec normalize_lang(config()) -> sheldon_dictionary:language().
normalize_lang(Config) ->
  maps:get(lang, Config, default_lang()).

-spec normalize_ignore_words(config()) -> [string()].
normalize_ignore_words(Config) ->
  IgnoredWords = maps:get(ignore_words, Config, default_ignore_words()),
  lists:map(fun string:to_lower/1, IgnoredWords).

-spec normalize_ignore_patterns(config()) -> [string()].
normalize_ignore_patterns(Config) ->
  maps:get(ignore_patterns, Config, default_ignore_patterns()).

-spec normalize_ignore_blocks(config()) -> [ignore_block()].
normalize_ignore_blocks(Config) ->
  maps:get(ignore_blocks, Config, default_ignore_blocks()).

-spec default_lang() -> sheldon_dictionary:language().
default_lang() -> eng.

-spec default_ignore_words() -> [].
default_ignore_words() -> [].

-spec default_ignore_patterns() -> [].
default_ignore_patterns() -> [].

-spec default_ignore_blocks() -> [].
default_ignore_blocks() -> [].
