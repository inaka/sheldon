%%% @doc Utils module for sheldon.
%%%
%%% Copyright 2016 Inaka &lt;hello@inaka.net&gt;
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
-module(sheldon_utils).
-author("Felipe Ripoll <ferigis@gmail.com>").

%% API
-export([
          normalize/1
        , can_be_number/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec normalize(string()) -> string().
normalize(Word) ->
  CharToScape = [
                  "\n"
                , "."
                , ","
                , ":"
                , ";"
                , "?"
                , ")"
                , "("
                , "\""
                , "\'"
                , "!"
                , "["
                , "]"
                , "{"
                , "}"
                ],
  Word1 = escape_chars(Word, CharToScape),
  [WordBin | _] = re:split(Word1, "'s"),
  binary_to_list(WordBin).

-spec can_be_number(string()) -> boolean().
can_be_number(Word) ->
  case re:run(Word, "^[0-9]*$") of
    nomatch -> false;
    _ -> true
  end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec escape_chars(string(), [string()]) -> string().
escape_chars(Word, []) -> Word;
escape_chars(Word, [Character | Rest]) ->
  [Word1 | _] = string:tokens(Word, Character),
  escape_chars(Word1, Rest).
