%%% @doc Markdown Adapter.
%%%
%%% Copyright 2017 Inaka &lt;hello@inaka.net&gt;
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
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(markdown_adapter).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% API
-export([adapt/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec adapt(binary()) -> iodata().
adapt(Line) ->
  {ok, Line1, _} = 'Elixir.Earmark':as_html(Line),
  % Earmark replace ' by ’ and ‘ which causes a bitstring, so we replace it by ' again
  Line2 = binary:replace(Line1, [<<226, 128, 152>>, <<226, 128, 153>>], <<39>>, [global]),
  % replace “ and ” by "
  Line3 = binary:replace(Line2, [<<226, 128, 156>>, <<226, 128, 157>>], <<34>>, [global]),
  html_adapter:adapt(Line3).
