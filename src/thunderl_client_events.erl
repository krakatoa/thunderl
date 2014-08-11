-module(thunderl_client_events).
-behaviour(gen_event).

-export([handle_event/2]).
-export([init/1, handle_cast/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init([Pid]) ->
  io:format("adding feed ~p~n", [Pid]),
  {ok, Pid}.

handle_event(Event, Pid) ->
  io:format("Should notify: ~p~n", [Pid]),
  Pid ! {thunderl_event, Event},
  {ok, Pid}.

handle_cast(_Msg, State) ->
  {ok, State}.

handle_call(_Msg, State) ->
  {ok, ok, State}.

handle_info(_Msg, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
