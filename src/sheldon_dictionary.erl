%%% @doc This gen_server only creates the ets for the dictionary and
%%%      ets for bazingas. This module also provides the 'candidates' feature.
%%%      That feature returns the list of candidates when one word is
%%%      misspelled and it also manages an ets table.
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
-module(sheldon_dictionary).

-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(gen_server).

%% API
-export([start_link/1, member/2, dictionary_name/1, get_bazinga/1, candidates/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export_type([language/0]).

-type language() :: eng.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc starts the gen_server
-spec start_link(language()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Lang) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Lang], []).

%% @doc evaluates if a given string() is member of the dictionary
-spec member(string(), language()) -> boolean().
member(Word, Lang) ->
    DictName = dictionary_name(Lang),
    WordLower = string:to_lower(Word),
    persistent_term:get({DictName, list_to_binary(WordLower)}, []) =/= [].

%% @doc returns a bazinga from the ETS
-spec get_bazinga(language()) -> string().
get_bazinga(Lang) ->
    BazingaName = bazinga_name(Lang),
    Keys = persistent_term:get(BazingaName),
    lists:nth(
        rand:uniform(length(Keys)), Keys).

%% @doc returns the name of the dictionary given the language() as a
%%      parameter
-spec dictionary_name(language()) -> atom().
dictionary_name(Lang) ->
    Bin = <<(atom_to_binary(sheldon, utf8))/binary,
            "_",
            (atom_to_binary(Lang, utf8))/binary>>,
    binary_to_atom(Bin, utf8).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([language()]) -> {ok, State :: map()}.
init([Lang]) ->
    ok = learn_language(Lang),
    ok = set_bazingas(Lang),
    {ok, #{}}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State) ->
                     {reply, ok, State}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Request :: term(), State) -> {noreply, State}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State) -> {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: map()) ->
                   term().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, term()}, State, Extra :: term()) ->
                     {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec learn_language(language()) -> ok.
learn_language(Lang) ->
    LangSource = [code:priv_dir(sheldon), "/lang/", atom_to_list(Lang), "/dictionary.txt"],
    DictionaryName = dictionary_name(Lang),
    _Words = fill_ets(DictionaryName, LangSource),
    ok.

-spec set_bazingas(language()) -> ok.
set_bazingas(Lang) ->
    BazingaSource = [code:priv_dir(sheldon), "/lang/", atom_to_list(Lang), "/bazinga.txt"],
    BazingaName = bazinga_name(Lang),
    {ok, SourceBin} = file:read_file(BazingaSource),
    Words = re:split(SourceBin, "\n"),
    persistent_term:put(BazingaName, Words).

-spec bazinga_name(language()) -> atom().
bazinga_name(Lang) ->
    Bin = <<(atom_to_binary(bazinga, utf8))/binary,
            "_",
            (atom_to_binary(Lang, utf8))/binary>>,
    binary_to_atom(Bin, utf8).

-spec fill_ets(atom(), term()) -> [binary()].
fill_ets(EtsName, Source) ->
    {ok, SourceBin} = file:read_file(Source),
    Words = re:split(SourceBin, "\n"), % one word per line
    [persistent_term:put({EtsName, Word}, Word) || Word <- Words],
    Words.

%%%===================================================================
%%% Corrector Internal Funcions
%%%===================================================================

-spec candidates(string(), language()) -> [string()].
candidates(WordStr, Lang) ->
    Word = list_to_binary(string:to_lower(WordStr)),
    Set1 = [Word],
    Set2 = edits1(Word),
    Set3 = edits2(Word),
    DictName = dictionary_name(Lang),
    Candidates = know(Set1 ++ Set2 ++ Set3, Lang, DictName, []),
    Candidates.

-spec know([binary()], language(), atom(), list()) -> [string()].
know([], _Lang, _, Acc) ->
    Acc;
know([Set | Sets], Lang, DictName, Acc) ->
    case persistent_term:get({DictName, Set}, undefined) of
        undefined ->
            know(Sets, Lang, DictName, Acc);
        Word ->
            know(Sets, Lang, DictName, [binary_to_list(Word) | Acc])
    end.

-spec edits1(binary()) -> [binary()].
edits1(WordBinary) ->
    Word = binary_to_list(WordBinary),
    Splits = [lists:split(I, Word) || I <- lists:seq(0, length(Word))],
    Acc1 = deletes(Splits, []),
    Acc2 = transposes(Splits, Acc1),
    Acc3 = replaces(Splits, Acc2),
    Acc4 = inserts(Splits, Acc3),
    lists:flatten(Acc4).

-spec deletes([tuple()], list()) -> list().
deletes(Splits, Acc) ->
    Result = [iolist_to_binary([Left, Right]) || {Left, [_ | Right]} <- Splits],
    [Result | Acc].

-spec transposes([tuple()], list()) -> list().
transposes(Splits, Acc) ->
    Result = [iolist_to_binary([Left, B, A, Right]) || {Left, [A, B | Right]} <- Splits],
    [Result | Acc].

-spec replaces([tuple()], list()) -> list().
replaces(Splits, Acc) ->
    Result =
        [iolist_to_binary([Left, Char, Right]) || {Left, [_ | Right]} <- Splits, Char <- chars()],
    [Result | Acc].

-spec inserts([tuple()], list()) -> list().
inserts(Splits, Acc) ->
    Result =
        [iolist_to_binary([Left, Char, Right]) || {Left, Right} <- Splits, Char <- chars()],
    [Result | Acc].

-spec edits2(binary()) -> [binary()].
edits2(Word) ->
    Result = [edits1(E1) || E1 <- edits1(Word)],
    lists:flatten(Result).

-spec chars() -> string().
chars() ->
    "abcdefghijklmnopqrstuvwxyz-".
