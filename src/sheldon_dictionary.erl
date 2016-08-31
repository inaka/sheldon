%%% @doc This gen_server only creates the ets for the dictionary and
%%%      ets for bazingas.
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
-module(sheldon_dictionary).
-author("Felipe Ripoll <ferigis@gmail.com>").

-behaviour(gen_server).

%% API
-export([ start_link/1
        , exist/2
        , dictionary_name/1
        , get_bazinga/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).


-export_type([ language/0
             ]).

-type language() :: eng.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(language()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Lang) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Lang], []).

-spec exist(string(), language()) -> boolean().
exist(Word, Lang) ->
  DictName = dictionary_name(Lang),
  ets:lookup(DictName, string:to_lower(Word)) =/= [].

-spec get_bazinga(language()) -> string().
get_bazinga(Lang) ->
  BazingaName = bazinga_name(Lang),
  Keys = ets:tab2list(BazingaName),
  {Bazinga} = lists:nth(random:uniform(length(Keys)), Keys),
  Bazinga.

-spec dictionary_name(language()) -> atom().
dictionary_name(Lang) ->
  Bin = << (atom_to_binary(sheldon, utf8))/binary
         , "_"
         , (atom_to_binary(Lang, utf8))/binary>>,
  binary_to_atom(Bin, utf8).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([language()]) -> {ok, State :: map()}.
init([Lang]) ->
  ok = learn_language(Lang),
  ok = set_bazingas(Lang),
  {ok, #{}}.

-spec handle_call( Request :: term()
                 , From    :: {pid()
                 , Tag     :: term()}
                 , State
                 ) -> {reply, ok, State}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(Request :: term(), State) ->
  {noreply, State}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State) ->
  {noreply, State}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate( Reason :: (normal | shutdown | {shutdown, term()} | term())
               , State  :: map()
               ) -> term().
terminate(_Reason, _State) ->
  ok.

-spec code_change(OldVsn :: term() | {down, term()}
                 , State
                 , Extra :: term()
                 ) -> {ok, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec learn_language(language()) -> ok.
learn_language(Lang) ->
  LangSource = [ code:priv_dir(sheldon)
               , "/lang/"
               , atom_to_list(Lang)
               , "/dictionary.txt"
               ],
  DictionaryName = dictionary_name(Lang),
  ok = fill_ets(DictionaryName, LangSource),
  ok.

-spec set_bazingas(language()) -> ok.
set_bazingas(Lang) ->
  BazingaSource = [ code:priv_dir(sheldon)
                  , "/lang/"
                  , atom_to_list(Lang)
                  , "/bazinga.txt"
                  ],
  BazingaName = bazinga_name(Lang),
  ok = fill_ets(BazingaName, BazingaSource),
  ok.

-spec bazinga_name(language()) -> atom().
bazinga_name(Lang) ->
  Bin = << (atom_to_binary(bazinga, utf8))/binary
         , "_"
         , (atom_to_binary(Lang, utf8))/binary>>,
  binary_to_atom(Bin, utf8).

-spec fill_ets(atom(), term()) -> ok.
fill_ets(EtsName, Source) ->
  {ok, SourceBin} = file:read_file(Source),
  SourceString = binary_to_list(SourceBin),
  Words = string:tokens(SourceString, "\n"),
  TableName = ets:new(EtsName, [named_table, {read_concurrency, true}]),
  [ets:insert(TableName, {Word}) || Word <- Words],
  ok.
