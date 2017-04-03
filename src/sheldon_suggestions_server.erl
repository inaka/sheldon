-module(sheldon_suggestions_server).

-behaviour(gen_server).

%%% API
-export([ suggestions/3
        , wait_suggestions/1
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

-spec suggestions(string(), sheldon_dictionary:language(), reference()) -> ok.
suggestions(Word, Lang, Ref) ->
  wpool:cast(suggestions_pool, {suggest, Word, Lang, Ref, self()}),
  ok.

-spec wait_suggestions([{reference(), sheldon_result:misspelled_word()}]) ->
  [sheldon_result:misspelled_word()].
wait_suggestions(Refs) ->
  Refs1 = lists:reverse(Refs),
  wait_suggestions(Refs1, []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-spec init(no_args) -> {ok, map()}.
init(no_args) ->
  {ok, #{}}.

-spec handle_call(term(), term(), map()) -> {reply, ok, map()}.
handle_call(_, _, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({suggest, Word, Lang, Ref, From}, State) ->
  From ! {Ref, sheldon_dictionary:candidates(Word, Lang)},
  {noreply, State};
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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec wait_suggestions( [{reference(), sheldon_result:misspelled_word()}]
                        , [sheldon_result:misspelled_word()]
                        ) -> [sheldon_result:misspelled_word()].
wait_suggestions([], Result) ->
  Result;
wait_suggestions([{Ref, Result} | Rest], Acc) ->
  receive
    {Ref, Candidates} ->
      wait_suggestions(Rest, [Result#{candidates => Candidates} | Acc])
  end.
