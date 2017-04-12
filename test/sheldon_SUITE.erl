-module(sheldon_SUITE).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ basic_check/1
        , iodata_check/1
        , ignore_words/1
        , ignore_patterns/1
        , multiline/1
        , ignore_blocks/1
        , suggest_words/1
        , markdown_adapter/1
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
          , ignore_blocks
          , suggest_words
          , markdown_adapter
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
  #{ misspelled_words := [#{ line_number := 1, word := "wrongspelled" }] } =
    sheldon:check("I am testing a wrongspelled word"),
  ok = sheldon:check("Hi, I am (testing the spelling) checker"),
  ok = sheldon:check("Hi, I am \"testing the\" spelling checker"),
  ok = sheldon:check("Hi, I am \'testing\' the spelling checker"),
  ok = sheldon:check("Hi, I am \'testing\' the.,; spelling checker"),
  #{ misspelled_words := [#{ line_number := 1, word := "thedfs" }] } =
    sheldon:check("Hi, I am \'testing\' thedfs.,; spelling checker"),
  ok = sheldon:check(""),
  ok = sheldon:check("I am checking numbers too 12, 34, 111, yeah!"),
  ok = sheldon:check("I am checking numbers too [12, 34], {111}, yeah!"),
  ok = sheldon:check("                                hello"),
  #{ misspelled_words := [#{ line_number := 1, word := "Sheldon" }] } =
    sheldon:check("Sheldon doesn't know his name, too bad"),
  ok = sheldon:check("I am checking numbers too [12, 34], {111}, yeah!", #{}),
  ok.

-spec iodata_check(config()) -> ok.
iodata_check(_Config) ->
  #{misspelled_words := [#{ line_number := 1, word := "iodata" }]} =
    sheldon:check(["I am",  [<<" testing with">>, " "] , <<"iodata">>]),
  ok = sheldon:check(["Hi, I am (testing the spelling) checker"]),
  ok = sheldon:check([ $H
                     , [<<"i">>, " I am"]
                     , <<" (testing the spelling) checker !">>
                     ]),
  #{ misspelled_words := [#{ line_number := 1, word := "thedfs" }] } =
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
  Config = #{ ignore_words => ["Sheldon"] },
  ok = sheldon:check("Sheldon doesn't know his name, too bad", Config),
  Config2 = #{ ignore_words => ["ttttthis", "sheldon", "differentone"] },
  ok =
    sheldon:check(<<"testing again. Sheldon should skip ttttthis">>, Config2),
  #{ misspelled_words := [#{ line_number := 1, word := "vvvvv" }] } =
    sheldon:check(<<"testing again. Sheldon should skip ttttthis"
                   , " but sheldon doesn't skip vvvvv">>, Config2),
  ok.

-spec ignore_patterns(config()) -> ok.
ignore_patterns(_Config) ->
  Config = #{ ignore_patterns => ["^begin"] },
  ok = sheldon:check("begindddd should be ignored", Config),
  Config2 = #{ ignore_patterns => ["^[4-6]", "^begin"] },
  ok = sheldon:check("begindddd and 4rrrrrr should be ignored", Config2),
  #{ misspelled_words := [#{ line_number := 1, word := "7rrrr" }] } =
    sheldon:check([ "begindddd and 4rrrrrr should be ignored"
                  , " but 7rrrr shouldn't be ignored, good..."
                  ], Config2),
  ok.

-spec multiline(config()) -> ok.
multiline(_Config) ->
  {ok, Bin} = file:read_file([ code:priv_dir(sheldon)
                             , "/../test/files/multiline.txt"
                             ]),
  #{ bazinga := _
   , misspelled_words :=
     [ #{ line_number := 1, word := "sheldon" }
     , #{ line_number := 1, word := "multiline" }
     , #{ line_number := 2, word := "fsdfdsd" }
     , #{ line_number := 3, word := "superwrong" }
     ]
   } = sheldon:check(Bin),
  #{ bazinga := _
   , misspelled_words :=
     [ #{ line_number := 1
        , word        := "multiline"
        }
     ]
   } = sheldon:check(Bin, #{ ignore_words => [ "Sheldon"
                                             , "superwrong"
                                             , "fsdfdsd"
                                             ]}),
  ok = sheldon:check(Bin, #{ ignore_words => [ "Sheldon"
                                             , "superwrong"
                                             , "fsdfdsd"
                                             , "multiline"
                                             ]}),
  ok.

-spec ignore_blocks(config()) -> ok.
ignore_blocks(_Config) ->
  {ok, BlockInline} = file:read_file([ code:priv_dir(sheldon)
                                     , "/../test/files/block_inline.txt"
                                     ]),
  #{ bazinga := _
   , misspelled_words :=
    [ #{ line_number := 2, word := "mistakke" }
    , #{ line_number := 4, word := "open_block" }
    , #{ line_number := 4, word := "fasd" }
    , #{ line_number := 4, word := "dsfas" }
    , #{ line_number := 4, word := "dfadf" }
    , #{ line_number := 4, word := "adsfsa" }
    , #{ line_number := 4, word := "close_block" }
    , #{ line_number := 7, word := "miiisstake" }
    ]
  } = sheldon:check(BlockInline),
  #{ bazinga := _
   , misspelled_words :=
    [ #{ line_number := 2, word := "mistakke" }
    , #{ line_number := 7, word := "miiisstake" }
    ]
  } = sheldon:check(BlockInline, #{ignore_blocks =>
                                   [#{ open => "^open_block$"
                                     , close => "^close_block$"
                                     }]
                                  }),
  {ok, BlockMultiLine} =
    file:read_file([ code:priv_dir(sheldon)
                   , "/../test/files/block_multiline.txt"
                   ]),
  #{ bazinga := _
   , misspelled_words :=
    [ #{ line_number := 2, word := "mistakke" }
    , #{ line_number := 4, word := "open_block" }
    , #{ line_number := 5, word := "fasd" }
    , #{ line_number := 6, word := "dsfas" }
    , #{ line_number := 6, word := "dfadf" }
    , #{ line_number := 7, word := "adsfsa" }
    , #{ line_number := 7, word := "close_block" }
    , #{ line_number := 7, word := "oneee" }
    , #{ line_number := 10, word := "miiisstake" }
    ]
  } = sheldon:check(BlockMultiLine),

  #{ bazinga := _
   , misspelled_words :=
    [ #{ line_number := 2, word := "mistakke" }
    , #{ line_number := 7, word := "oneee" }
    , #{ line_number := 10, word := "miiisstake" }
    ]
  } = sheldon:check(BlockMultiLine, #{ignore_blocks =>
                                      [#{ open => "^open_block$"
                                        , close => "^close_block$"
                                        }]
                                     }),

  {ok, BlockMultiLine2} =
    file:read_file([ code:priv_dir(sheldon)
                   , "/../test/files/block_multiline_no_end.md"
                   ]),
  #{ bazinga := _
   , misspelled_words :=
    [ #{ line_number := 2, word := "mistakke" }
    , #{ line_number := 4, word := "open_block" }
    , #{ line_number := 5, word := "fasd" }
    , #{ line_number := 6, word := "dsfas" }
    , #{ line_number := 6, word := "dfadf" }
    , #{ line_number := 7, word := "adsfsa" }
    , #{ line_number := 7, word := "oneee" }
    , #{ line_number := 10, word := "miiisstake" }
    ]
   } = sheldon:check(BlockMultiLine2),

  #{ bazinga := _
   , misspelled_words :=
    [ #{ line_number := 2, word := "mistakke"}
    ]
   } = sheldon:check(BlockMultiLine2, #{ignore_blocks =>
                                        [#{ open => "^open_block$"
                                          , close => "^close_block$"
                                          }]
                                       }),
  ok.

-spec suggest_words(config()) -> ok.
suggest_words(_Config) ->
  ok = sheldon:check("Hi, I am testing the spelling checker again"),
  ok = sheldon:check("lets check the suggestor"),
  #{ bazinga := _
   , misspelled_words :=
    [#{candidates := Candidates, line_number := 1, word := "speling"}]
   } = sheldon:check("speling is wrong, should suggest spelling among others"),
  true = lists:member("spelling", Candidates),
  #{ bazinga := _
   , misspelled_words :=
    [#{candidates := [], line_number := 1, word := "this_is_wrong"}]
   } = sheldon:check("The next one is this_is_wrong, no suggestions for that"),
  ok.

-spec markdown_adapter(config()) -> ok.
markdown_adapter(_Config) ->
  {ok, MarkownFile} =
    file:read_file([ code:priv_dir(sheldon)
                   , "/../test/files/markdown.md"
                   ]),

  #{ bazinga := _
   , misspelled_words :=
     [ #{ line_number := 1, word := "#" }
     , #{ line_number := 1, word := "API" }
     , #{ line_number := 3, word := "##" }
     , #{ line_number := 7, word := "##" }
     , #{ line_number := 9, word := "**questions**" }
     , #{ line_number := 9, word := "**general" }
     , #{ line_number := 9, word := "comments**" }
     , #{ line_number := 10, word := "hipchat" }
     , #{ line_number := 12, word := "open-source" }
     , #{ line_number := 14, word := "##" }
     , #{ line_number := 16, word := "####" }
     , #{ line_number := 18, word := "API" }
     , #{ line_number := 20, word := "open-source" }
     , #{ line_number := 22, word := "start-block" }
     , #{ line_number := 24, word := "end-block" }
     , #{ line_number := 27, word := "*another" }
     , #{ line_number := 27, word := "block*" }
     , #{ line_number := 27, word := "start-block1" }
     , #{ line_number := 28, word := "end-block1" }
     , #{ line_number := 28, word := "start-block1" }
     , #{ line_number := 28, word := "end-block1" }
     , #{ line_number := 28, word := "start-block" }
     , #{ line_number := 29, word := "end-block" }
     , #{ line_number := 33, word := "##" }
     ]
   } = sheldon:check(MarkownFile, #{ignore_patterns => ["http://"]}),

  #{ bazinga := _
   , misspelled_words :=
     [ #{ line_number := 1, word := "API" }
     , #{ line_number := 4, word := "Inaka" }
     , #{ line_number := 5, word := "Jayme" }
     , #{ line_number := 5, word := "Dayron" }
     , #{ line_number := 5, word := "SumoREST" }
     , #{ line_number := 10, word := "hipchat" }
     , #{ line_number := 12, word := "open-source" }
     , #{ line_number := 12, word := "inaka.github.io" }
     , #{ line_number := 18, word := "API" }
     , #{ line_number := 20, word := "open-source" }
     , #{ line_number := 20, word := "inaka.github.io" }
     , #{ line_number := 22, word := "start-block" }
     , #{ line_number := 24, word := "end-block" }
     , #{ line_number := 27, word := "start-block1" }
     , #{ line_number := 28, word := "end-block1" }
     , #{ line_number := 28, word := "start-block1" }
     , #{ line_number := 28, word := "end-block1" }
     , #{ line_number := 28, word := "start-block" }
     , #{ line_number := 29, word := "end-block" }
     ]
   } = sheldon:check(MarkownFile, #{ adapters => [markdown_adapter] }),

  #{ bazinga := _
   , misspelled_words :=
     [ #{ line_number := 1, word := "API" }
     , #{ line_number := 4, word := "Inaka" }
     , #{ line_number := 5, word := "Jayme" }
     , #{ line_number := 5, word := "Dayron" }
     , #{ line_number := 5, word := "SumoREST" }
     , #{ line_number := 10, word := "hipchat" }
     , #{ line_number := 12, word := "open-source" }
     , #{ line_number := 12, word := "inaka.github.io" }
     , #{ line_number := 18, word := "API" }
     , #{ line_number := 20, word := "open-source" }
     , #{ line_number := 20, word := "inaka.github.io" }
     ]
   } = sheldon:check(MarkownFile, #{ adapters => [markdown_adapter]
                                   , ignore_blocks =>
                                       [ #{ open => "^start-block$", close => "^end-block$" }
                                       , #{ open => "^start-block1$", close => "^end-block1$" }
                                       ]
                                   }),

  ok = sheldon:check( MarkownFile
                    , #{ adapters => [markdown_adapter]
                       , ignore_blocks =>
                           [ #{ open => "^start-block$", close => "^end-block$" }
                           , #{ open => "^start-block1$", close => "^end-block1$" }
                           ]
                       , ignore_words =>
                           [ "api"
                           , "inaka"
                           , "inaka.github.io"
                           , "sumorest"
                           , "dayron"
                           , "jayme"
                           , "hipchat"
                           , "open-source"
                           ]
                       }),

   ok.
