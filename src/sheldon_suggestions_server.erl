-module(sheldon_suggestions_server).

-behaviour(gen_server).

%%% API
-export([ suggestions/2
        , add_suggestions/2
        ]).

%%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec suggestions([sheldon_result:misspelled_word()], sheldon_dictionary:language()) ->
  [sheldon_result:misspelled_word()].
suggestions(MisspelledWords, Lang) ->
  try rpc:pmap({?MODULE, add_suggestions}, [Lang], MisspelledWords) of
    Result -> Result
  catch
    exit:badrpc -> MisspelledWords
  end.

-spec add_suggestions(sheldon_result:misspelled_word(), sheldon_dictionary:language()) ->
  sheldon_result:misspelled_word().
add_suggestions(#{word := Word} = MisspelledWord, Lang) ->
  Candidates = wpool:call(suggestions_pool, {suggest, Word, Lang}, available_worker, infinity),
  MisspelledWord#{candidates => Candidates}.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-spec init(no_args) -> {ok, map()}.
init(no_args) ->
  {ok, #{}}.

-spec handle_call(term(), term(), map()) -> {reply, ok, map()}.
handle_call({suggest, Word, Lang}, _From, State) ->
  Candidates = sheldon_dictionary:candidates(Word, Lang),
  {reply, Candidates, State};
handle_call(_, _, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
  ok.

%% @hidden
-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
