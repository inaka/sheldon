-module(sheldon_SUITE).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

-export([
  code_coverage/1
]).

-define(EXCLUDED_FUNS,
  [
    module_info,
    all,
    init_per_suite,
    end_per_suite
  ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

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

-spec code_coverage(config()) -> ok.
code_coverage(_Config) ->
  ok.
