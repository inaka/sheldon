-module(sheldon_meta_SUITE).

-dialyzer(no_behaviours).

-behaviour(ct_suite).

-include_lib("mixer/include/mixer.hrl").

-mixin([{ktn_meta_SUITE, [all/0, xref/1, dialyzer/1, elvis/1]}]).

-export([init_per_suite/1, end_per_suite/1]).

-type config() :: [{atom(), term()}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    [{application, sheldon}, {dialyzer_warnings, [no_missing_calls, no_undefined_callbacks]}]
    ++ Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    Config.
