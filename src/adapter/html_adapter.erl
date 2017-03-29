%%% @doc HTML Adapter. *Note* This is not the final version of html_adapter, currently it only
%%% escapes tags and replace some special html characters.
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
-module(html_adapter).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% API
-export([adapt/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec adapt(binary()) -> iodata().
adapt(Line) ->
  Line1 = escape_tags(Line),
  replace_chars(Line1).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec replace_chars(binary()) -> iodata().
replace_chars(Line) ->
  Chars = chars_to_replace(),
  replace_chars(Line, Chars).

-spec replace_chars(iodata(), [{string(), string()}]) -> iodata().
replace_chars(Line, []) ->
  Line;
replace_chars(Line, [{Pattern, ReplaceBy} | Rest]) ->
  Result = re:replace(Line, Pattern, ReplaceBy),
  Result2 = replace(Result, Line, {Pattern, ReplaceBy}),
  replace_chars(Result2, Rest).

-spec escape_tags(iodata()) -> iodata().
escape_tags(Line) ->
  Pattern = "<[^>]*>",
  ReplaceBy = "",
  Result = re:replace(Line, Pattern, ReplaceBy),
  replace(Result, Line, {Pattern, ReplaceBy}).

-spec replace(iodata(), iodata(), {string(), string()}) -> iodata().
replace(Line, Line, _) ->
  Line;
replace([Line, []], _, _) ->
  Line;
replace(Line, _PreviousLine, {Pattern, ReplaceBy}) ->
  Result = re:replace(Line, Pattern, ReplaceBy),
  replace(Result, Line, {Pattern, ReplaceBy}).

-spec chars_to_replace() -> [{string(), string()}].
chars_to_replace() ->
  [ {"&quot;", "\""}
  , {"&amp;", "&"}
  , {"&lt;", "<"}
  ].
