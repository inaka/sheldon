-module(example).

-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([check_file/1, check_file/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_file(string()) -> sheldon_result:result().
check_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    sheldon:check(Bin).

-spec check_file(string(), sheldon_config:config()) -> sheldon_result:result().
check_file(Path, Config) ->
    {ok, Bin} = file:read_file(Path),
    sheldon:check(Bin, Config).
