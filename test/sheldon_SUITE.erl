-module(sheldon_SUITE).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([
          all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([
          basic_check/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [
            basic_check
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
  #{result := ["Sheldon"]} =
    sheldon:check("Sheldon doesn't know his name, too bad"),
  ok.
