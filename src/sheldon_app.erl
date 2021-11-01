%%% @hidden
%%% @doc Sheldon's Application behaviour.
%%%
%%% Copyright Erlang Solutions Ltd. 217 &lt;hello@inaka.net&gt;
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
-module(sheldon_app).

-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(term(), term()) -> {error, term()} | {ok, pid()}.
start(_StartType, _StartArgs) ->
    sheldon_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
