-module(sheldon_SUITE).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ basic_check/1
        , iodata_check/1
        , ignore_words/1
        , ignore_patterns/1
        , multiline/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ basic_check
          , iodata_check
          , ignore_words
          , ignore_patterns
          , multiline
          ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  ok = sheldon:start(),
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
  #{misspelled_words := [#{word := "wrongspelled"}]} =
    sheldon:check("I am testing a wrongspelled word"),
  ok = sheldon:check("Hi, I am (testing the spelling) checker"),
  ok = sheldon:check("Hi, I am \"testing the\" spelling checker"),
  ok = sheldon:check("Hi, I am \'testing\' the spelling checker"),
  ok = sheldon:check("Hi, I am \'testing\' the.,; spelling checker"),
  #{misspelled_words := [#{word := "thedfs"}]} =
    sheldon:check("Hi, I am \'testing\' thedfs.,; spelling checker"),
  ok = sheldon:check(""),
  ok = sheldon:check("I am checking numbers too 12, 34, 111, yeah!"),
  ok = sheldon:check("I am checking numbers too [12, 34], {111}, yeah!"),
  ok = sheldon:check("                                hello"),
  #{ misspelled_words := [#{word := "Sheldon"}]} =
    sheldon:check("Sheldon doesn't know his name, too bad"),
  ok = sheldon:check("I am checking numbers too [12, 34], {111}, yeah!", #{}),
  ok.

-spec iodata_check(config()) -> ok.
iodata_check(_Config) ->
  #{misspelled_words := [#{word := "iodata"}]} =
    sheldon:check(["I am",  [<<" testing with">>, " "] , <<"iodata">>]),
  ok = sheldon:check(["Hi, I am (testing the spelling) checker"]),
  ok = sheldon:check([ $H
                     , [<<"i">>, " I am"]
                     , <<" (testing the spelling) checker">>
                     ]),
  #{misspelled_words := [#{word := "thedfs"}]} =
    sheldon:check([ "Hi, I am \'tes"
                  , <<"ting\' the">>
                  , $d
                  , $f
                  , "s.,; spelling checker"
                  ]),
  ok = sheldon:check(<<"Hi, I am (testing the spelling) checker">>),
  ok.

-spec ignore_words(config()) -> ok.
ignore_words(_Config) ->
  Config = #{ignore_words => ["Sheldon"] },
  ok = sheldon:check("Sheldon doesn't know his name, too bad", Config),
  Config2 = #{ignore_words => ["ttttthis", "sheldon", "differentone"] },
  ok =
    sheldon:check(<<"testing again. Sheldon should skip ttttthis">>, Config2),
  #{misspelled_words := [#{word := "vvvvv"}]} =
    sheldon:check(<<"testing again. Sheldon should skip ttttthis"
                   , " but sheldon doesn't skip vvvvv">>, Config2),
  ok.

-spec ignore_patterns(config()) -> ok.
ignore_patterns(_Config) ->
  Config = #{ignore_patterns => ["^begin"]},
  ok = sheldon:check("begindddd should be ignored", Config),
  Config2 = #{ignore_patterns => ["^[4-6]", "^begin"]},
  ok = sheldon:check("begindddd and 4rrrrrr should be ignored", Config2),
  #{misspelled_words := [#{word := "7rrrr"}]} =
    sheldon:check([ "begindddd and 4rrrrrr should be ignored"
                  , " but 7rrrr shouldn't be ignored, good..."
                  ], Config2),
  ok.

-spec multiline(config()) -> ok.
multiline(_Config) ->
  {ok, Bin} = file:read_file([ code:priv_dir(sheldon)
                             , "/../test/files/multiline.txt"
                             ]),
  #{ bazinga := _Bazinga
   , misspelled_words :=
     [ #{ line_number := 3
        , word        := "superwrong"
        }
     , #{ line_number := 2
        , word        := "fsdfdsd"
        }
     , #{ line_number := 1
        , word        := "multiline"
        }
     , #{ line_number := 1
        , word        := "["
        }
     , #{ line_number := 1
        , word        := "sheldon"
        }
     ]
   } = sheldon:check(Bin),
  #{ bazinga := _
   , misspelled_words :=
     [ #{ line_number := 1
        , word        := "multiline"
        }
     ]
   } = sheldon:check(Bin, #{ignore_words => [ "Sheldon"
                                            , "superwrong"
                                            , "fsdfdsd"
                                            , "["
                                            ]}),
  ok = sheldon:check(Bin, #{ignore_words => [ "Sheldon"
                                            , "superwrong"
                                            , "fsdfdsd"
                                            , "["
                                            , "multiline"
                                            ]}),
  ok.
