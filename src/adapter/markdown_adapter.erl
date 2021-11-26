%%% @doc Markdown Adapter.
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
-module(markdown_adapter).

-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(sheldon_adapter).

%% API
-export([adapt/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec adapt(binary()) -> iodata().
adapt(Line) ->
    %% Prepare links
    L0 = binary:replace(Line,
                        [<<"`">>, <<"[">>, <<"]">>, <<"(">>, <<")">>],
                        <<" ">>,
                        [global]),
    %% Delete links in line
    L1 = re:replace(L0, "(http\S*?)(\.*?\s)", " ", [global, {return, binary}]),
    %% Delete other symbols
    binary:replace(L1, [<<"`">>, <<"<">>, <<">">>, <<"*">>, <<"#">>], <<>>, [global]).
