-module(sheldon_coverage_SUITE).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ sheldon_suggestions_server/1
        , sheldon_dictionary/1
        , mapsets/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ sheldon_suggestions_server
          , sheldon_dictionary
          , mapsets
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

-spec sheldon_suggestions_server(config()) -> ok.
sheldon_suggestions_server(_Config) ->
  State = #{},
  {reply, ok, State} = sheldon_suggestions_server:handle_call(hello, {self(), tag}, State),
  {noreply, State} = sheldon_suggestions_server:handle_cast(hello, State),
  {noreply, State} = sheldon_suggestions_server:handle_info(hello, State),
  ok = sheldon_suggestions_server:terminate(reason, State),
  {ok, State} = sheldon_suggestions_server:code_change(old_vsn, State, extra),
  ok.

-spec sheldon_dictionary(config()) -> ok.
sheldon_dictionary(_Config) ->
  State = #{},
  {reply, ok, State} = sheldon_dictionary:handle_call(hello, {self(), tag}, State),
  {noreply, State} = sheldon_dictionary:handle_cast(hello, State),
  {noreply, State} = sheldon_dictionary:handle_info(hello, State),
  ok = sheldon_dictionary:terminate(reason, State),
  {ok, State} = sheldon_dictionary:code_change(old_vsn, State, extra),
  ok.

-spec mapsets(config()) -> ok.
mapsets(_Config) ->
  MapSet1 = mapsets:add_element(a, mapsets:new()),
  MapSet1b = mapsets:add_element(b, MapSet1),
  MapSet2 = mapsets:add_element(b, mapsets:new()),
  MapSet2 = mapsets:intersection(MapSet1b, MapSet2),
  ok.
