-module(sheldon_dictionaries_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([default_dictionary/1, additional_dictionary/1]).

%% =============================================================================
%% Common test
%% =============================================================================

-spec all() -> [atom()].
all() ->
    [default_dictionary, additional_dictionary].

-spec init_per_suite(ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_suite(Config) ->
    Dictionary = [code:priv_dir(sheldon), "/../test/dictionaries/default_dictionary.txt"],
    ok = application:set_env(sheldon, default_dictionary, Dictionary),
    Config.

-spec end_per_suite(ct_suite:ct_config()) -> ct_suite:ct_config().
end_per_suite(Config) ->
    Config.

-spec init_per_testcase(ct_suite:ct_testname(), ct_suite:ct_config()) ->
                           ct_suite:ct_config() | {fail, term()} | {skip, term()}.
init_per_testcase(default_dictionary, Config) ->
    ok = sheldon:start(),
    Config;
init_per_testcase(additional_dictionary, Config) ->
    One = [code:priv_dir(sheldon), "/../test/dictionaries/additional_dictionary_1.txt"],
    Two = [code:priv_dir(sheldon), "/../test/dictionaries/additional_dictionary_2.txt"],
    ok = application:set_env(sheldon, additional_dictionaries, [One, Two]),
    ok = sheldon:start(),
    Config;
init_per_testcase(_Name, Config) ->
    Config.

-spec end_per_testcase(ct_suite:ct_testname(), ct_suite:ct_config()) ->
                          term() | {fail, term()} | {save_config, ct_suite:ct_config()}.
end_per_testcase(_Name, _Config) ->
    ok = application:stop(sheldon),
    ok.

%% =============================================================================
%% Test Cases
%% =============================================================================

-spec default_dictionary(ct_suite:ct_config()) -> ok | no_return().
default_dictionary(_Config) ->
    ok = sheldon:check("wikipedia"),
    #{bazinga := _} = sheldon:check("wiki"),
    ok.

-spec additional_dictionary(ct_suite:ct_config()) -> ok | no_return().
additional_dictionary(_Config) ->
    ok = sheldon:check("Wikipedia"),
    ok = sheldon:check("Wiki"),
    ok = sheldon:check("Fabuloso"),
    ok.
