-module(sheldon_coverage_SUITE).

-author("Felipe Ripoll <ferigis@gmail.com>").

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([sheldon_dictionary/1]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    [sheldon_dictionary].

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

-spec sheldon_dictionary(config()) -> ok.
sheldon_dictionary(_Config) ->
    State = #{},
    {reply, ok, State} = sheldon_dictionary:handle_call(hello, {self(), tag}, State),
    {noreply, State} = sheldon_dictionary:handle_cast(hello, State),
    {noreply, State} = sheldon_dictionary:handle_info(hello, State),
    ok = sheldon_dictionary:terminate(reason, State),
    {ok, State} = sheldon_dictionary:code_change(old_vsn, State, extra),
    ok.
