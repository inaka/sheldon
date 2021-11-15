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
    WordLower = string:lowercase(Word),
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
    fill_cashe(DictionaryName, LangSource).

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

-spec fill_cashe(atom(), term()) -> ok.
fill_cashe(Name, Source) ->
    SourceBin = generate_dictionary(Source),
    Words = re:split(SourceBin, "\n"), % one word per line
    Name = ets:new(Name, [named_table, bag, {read_concurrency, true}]),

    [begin
         persistent_term:put({Name, Word}, Word),
         String = binary_to_list(Word),
         [ets:insert(Name, {TypoWord, String}) || TypoWord <- typo_model(String)]
     end
     || Word <- Words],
    ok.

-spec candidates(string(), language()) -> [string()].
candidates(WordStr, Lang) ->
    Word = string:lowercase(WordStr),
    MaybeWords = typo_model(Word),
    DicName = dictionary_name(Lang),
    [V || {_, V} <- lookup(DicName, MaybeWords, [])].

%%%===================================================================
%%% Internal Funcions
%%%===================================================================

-spec lookup(atom(), [string()], list()) -> [tuple()].
lookup(_, [], Acc) ->
    lists:flatten(Acc);
lookup(DicName, [Word | Words], Acc) ->
    Result = ets:lookup(DicName, Word),
    lookup(DicName, Words, [Result | Acc]).

-spec typo_model(string()) -> [string()].
typo_model(Word) ->
    lists:foldl(fun ({Left, [_ | Right]}, Acc) ->
                        [lists:concat([Left, Right]) | Acc];
                    (_, Acc) ->
                        Acc
                end,
                [Word],
                [lists:split(I, Word) || I <- lists:seq(0, length(Word))]).

-spec generate_dictionary(file:name_all()) -> binary().
generate_dictionary(Source) ->
    DefaultDictionary = application:get_env(sheldon, default_dictionary, Source),
    AdditionalDictionaries = application:get_env(sheldon, additional_dictionaries, []),
    concat_dictionaries(DefaultDictionary, AdditionalDictionaries).

-spec concat_dictionaries(file:name_all(), [file:name_all()] | []) -> binary().
concat_dictionaries(Source, AdditionalDictionaries) ->
    {ok, SourceBin} = file:read_file(Source),
    lists:foldl(fun(Path, Acc) ->
                   {ok, Bin} = file:read_file(Path),
                   <<Acc/binary, "\n", Bin/binary>>
                end,
                SourceBin,
                AdditionalDictionaries).
