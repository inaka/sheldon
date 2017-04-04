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
  {ok, Line1, _} = 'Elixir.Earmark':as_html(Line, options()),
  html_adapter:adapt(Line1).

%%%===================================================================
%%% Internal Funcions
%%%===================================================================

-spec options() -> map().
options() ->
  #{ renderer          => 'Elixir.Earmark.HtmlRenderer'
   , gfm               => true
   , breaks            => false
   , pedantic          => false
   , smartypants       => false
   , sanitize          => false
   , footnotes         => false
   , footnote_offset   => 1
   , code_class_prefix => 'nil'
   , do_smartypants    => 'nil'
   , do_sanitize       => 'nil'
   , mapper            => fun 'Elixir.Earmark':'pmap'/2
   , render_code       => fun 'Elixir.Earmark.HtmlRenderer':'render_code'/1
   , file              => <<"<no file>">>
   , line              => 1
   , messages          => []
   , plugins           => #{}
   , '__struct__' => 'Elixir.Earmark.Options'
   }.
