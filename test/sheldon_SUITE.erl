-module(sheldon_SUITE).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ basic_check/1
        , iodata_check/1
        , validate_config/1
        , ignore_words/1
        , ignore_patterns/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ basic_check
          , iodata_check
          , validate_config
          , ignore_words
          , ignore_patterns
          ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(sheldon),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = application:stop(sheldon),
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec basic_check(config()) -> ok.
basic_check(_Config) ->
  ok = sheldon:check("Hi, I am testing the spelling checker"),
  #{result := ["wrongspelled"]} =
    sheldon:check("I am testing a wrongspelled word"),
  ok = sheldon:check("Hi, I am (testing the spelling) checker"),
  ok = sheldon:check("Hi, I am \"testing the\" spelling checker"),
  ok = sheldon:check("Hi, I am \'testing\' the spelling checker"),
  ok = sheldon:check("Hi, I am \'testing\' the.,; spelling checker"),
  #{result := ["thedfs"]} =
    sheldon:check("Hi, I am \'testing\' thedfs.,; spelling checker"),
  ok = sheldon:check(""),
  ok = sheldon:check("I am checking numbers too 12, 34, 111, yeah!"),
  ok = sheldon:check("I am checking numbers too [12, 34], {111}, yeah!"),
  ok = sheldon:check("                                hello"),
  #{result := ["Sheldon"]} =
    sheldon:check("Sheldon doesn't know his name, too bad"),
  ok = sheldon:check("I am checking numbers too [12, 34], {111}, yeah!", #{}),
  ok.

-spec iodata_check(config()) -> ok.
iodata_check(_Config) ->
  #{result := ["iodata"]} =
    sheldon:check(["I am",  [<<" testing with">>, " "] , <<"iodata">>]),
  ok = sheldon:check(["Hi, I am (testing the spelling) checker"]),
  ok = sheldon:check([ $H
                     , [<<"i">>, " I am"]
                     , <<" (testing the spelling) checker">>
                     ]),
  #{result := ["thedfs"]} =
    sheldon:check([ "Hi, I am \'tes"
                  , <<"ting\' the">>
                  , $d
                  , $f
                  , "s.,; spelling checker"
                  ]),
  ok = sheldon:check(<<"Hi, I am (testing the spelling) checker">>),
  ok.

-spec validate_config(config()) -> ok.
validate_config(_Config) ->
  {invalid_config, not_supported_lang} =
    (catch sheldon:check( "I want to check with a bad config file"
      , #{lang => es}
    )),
  {invalid_config, ignore_words_not_list} =
    (catch sheldon:check( "I want to check with a bad config file"
      , #{ignore_words => this_is_bad}
    )),
  {invalid_config, ignore_patterns_not_list} =
    (catch sheldon:check( "I want to check with a bad config file"
      , #{ignore_patterns => this_is_bad}
    )),
  Config = #{ lang            => eng
            , ignore_words    => ["this", "is", "good"]
            , ignore_patterns => ["this", "is", "good", "too"]
            },
  Config = sheldon_config:normalize(Config),
  ok.

-spec ignore_words(config()) -> ok.
ignore_words(_Config) ->
  Config = #{ignore_words => ["Sheldon"] },
  ok = sheldon:check("Sheldon doesn't know his name, too bad", Config),
  Config2 = #{ignore_words => ["ttttthis", "sheldon", "differentone"] },
  ok =
    sheldon:check(<<"testing again. Sheldon should skip ttttthis">>, Config2),
  #{result := ["vvvvv"]} =
    sheldon:check(<<"testing again. Sheldon should skip ttttthis"
                   , " but sheldon doesn't skip vvvvv">>, Config2),
  ok.

-spec ignore_patterns(config()) -> ok.
ignore_patterns(_Config) ->
  Config = #{ignore_patterns => ["^begin"]},
  ok = sheldon:check("begindddd should be ignored", Config),
  Config2 = #{ignore_patterns => ["^[4-6]", "^begin"]},
  ok = sheldon:check("begindddd and 4rrrrrr should be ignored", Config2),
  #{result := ["7rrrr"]} =
    sheldon:check([ "begindddd and 4rrrrrr should be ignored"
                  , " but 7rrrr shouldn't be ignored, good..."
                  ], Config2),
  ok.
