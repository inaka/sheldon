%%% @doc Main Sheldon's supervisor.
%%%
%%% Copyright Erlang Solutions Ltd. 2017 &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Erlang Solutions Ltd. <hello@inaka.net>
%%%
-module(sheldon_sup).

-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @doc here one child will be created per each dictionary
-spec init(any()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 1000,
          period => 3600},

    % Get all the languages, they are in /priv/lang/, each folder is a language
    {ok, SupportedLang} = file:list_dir(code:priv_dir(sheldon) ++ "/lang/"),

    Children = lists:map(fun get_child/1, SupportedLang),

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec get_child(string()) -> supervisor:child_spec().
get_child(LangString) ->
    Lang = list_to_atom(LangString),
    Id = sheldon_dictionary:dictionary_name(Lang),
    #{id => Id,
      start => {sheldon_dictionary, start_link, [Lang]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [sheldon_dictionary]}.
