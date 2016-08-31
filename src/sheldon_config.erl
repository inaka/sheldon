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

-type config() ::
  #{ lang            => sheldon_dictionary:language()
   , ignore_words    => [string()]
   , ignore_patterns => [string()]
   }.

%%%===================================================================
%%% API
%%%===================================================================

-spec default() -> config().
default() ->
  #{ lang            => default_lang()
   , ignore_words    => default_ignore_words()
   , ignore_patterns => default_ignore_patterns()
   }.

-spec normalize(config()) -> config().
normalize(Config) when is_map(Config) ->
  Config1 = normalize_lang(Config),
  Config2 = normalize_ignore_words(Config1),
  Config3 = normalize_ignore_patterns(Config2),
  Config3.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec normalize_lang(config()) -> config().
normalize_lang(Config) ->
  Lang = maps:get(lang, Config, default_lang()),
  case lists:member(Lang, lang_supported()) of
    true  -> Config#{lang => Lang};
    false -> throw({invalid_config, not_supported_lang})
  end.

-spec normalize_ignore_words(config()) -> config().
normalize_ignore_words(Config) ->
  IgnoreWords = maps:get(ignore_words, Config, default_ignore_words()),
  case is_list(IgnoreWords) of
    true  ->
      IgnoreWordsLower = lists:map(fun string:to_lower/1, IgnoreWords),
      Config#{ignore_words => IgnoreWordsLower};
    false -> throw({invalid_config, ignore_words_not_list})
  end.

-spec normalize_ignore_patterns(config()) -> config().
normalize_ignore_patterns(Config) ->
  Patterns = maps:get(ignore_patterns, Config, default_ignore_patterns()),
  case is_list(Patterns) of
    true  -> Config#{ignore_patterns => Patterns};
    false -> throw({invalid_config, ignore_patterns_not_list})
  end.

-spec default_lang() -> sheldon_dictionary:language().
default_lang() -> eng.

-spec default_ignore_words() -> [].
default_ignore_words() -> [].

-spec default_ignore_patterns() -> [].
default_ignore_patterns() -> [].

-spec lang_supported() -> [sheldon_dictionary:language()].
lang_supported() -> [eng].
