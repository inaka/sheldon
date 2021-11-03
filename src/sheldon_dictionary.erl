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

-define(LIMIT, 10).
-define(WORD_SIZE_LIMIT, 45).

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
    ets:lookup(DictName, WordLower) =/= [].

%% @doc returns a bazinga from the ETS
-spec get_bazinga(language()) -> string().
get_bazinga(Lang) ->
    BazingaName = bazinga_name(Lang),
    Keys = ets:tab2list(BazingaName),
    {Bazinga} =
        lists:nth(
            rand:uniform(length(Keys)), Keys),
    Bazinga.

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
    Words = fill_ets(DictionaryName, LangSource),

    % save the keys in set format in order to suggest words
    [ets:insert(DictionaryName, {binary_to_list(W)}) || W <- Words],
    ok.

-spec set_bazingas(language()) -> ok.
set_bazingas(Lang) ->
    BazingaSource = [code:priv_dir(sheldon), "/lang/", atom_to_list(Lang), "/bazinga.txt"],
    BazingaName = bazinga_name(Lang),
    _Bazingas = fill_ets(BazingaName, BazingaSource),
    ok.

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
    ok = create_ets(EtsName),
    ets:insert(EtsName, [{binary_to_list(Word)} || Word <- Words]),
    Words.

-spec create_ets(atom()) -> ok.
create_ets(EtsName) ->
    EtsName = ets:new(EtsName, [named_table, set, {read_concurrency, true}]),
    ok.

%%%===================================================================
%%% Corrector Internal Funcions
%%%===================================================================

-spec candidates(string(), language()) -> [string()].
candidates(WordStr, Lang) ->
    Word = string:to_lower(WordStr),
    know(Word, Lang).

-spec know(string(), language()) -> [string(), ...].
know(Word, Lang) ->
    know(Word, Lang, ?LIMIT).

-spec know(string(), language(), integer()) -> [string(), ...] | [].
know(Word, _, Attempt) when Word == []; Attempt == 0; length(Word) > ?WORD_SIZE_LIMIT ->
    [];
know(Word, Lang, Attempt) ->
    case ets:match(dictionary_name(Lang), {Word ++ '$1'}, ?LIMIT) of
        {Result, _} ->
            [lists:flatten(Word ++ R) || R <- Result];
        '$end_of_table' ->
            know(lists:droplast(Word), Lang, Attempt - 1)
    end.
