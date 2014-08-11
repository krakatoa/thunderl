-module(thunderl_call).
-behaviour(gen_server).

-export([create/3, answer/1, hangup/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("erim/include/exmpp_client.hrl").
-include_lib("erim/include/exmpp_xml.hrl").

create({UUID, From, To}, Children, Client) ->
  gen_server:start_link(?MODULE, {{UUID, From, To}, Children, Client}, []).

init({{UUID, From, To}, Children, Client}) ->
  io:format("From: ~p~n", [From]),
  io:format("To: ~p~n", [To]),
  {ok, {UUID, Client}}.

answer(Pid) ->
  gen_server:call(Pid, {answer}).
hangup(Pid) ->
  gen_server:call(Pid, {hangup}).

handle_call({answer}, _From, State) ->
  {UUID, Client} = State,
  delayed_ok = gen_server:call(Client, {answer_call, UUID}),
  {reply, ok, State};
handle_call({hangup}, _From, State) ->
  {UUID, Client} = State,
  delayed_ok = gen_server:call(Client, {hangup_call, UUID}),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({finish}, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  io:format("Unhandled message!~n~p", [Info]),
  {noreply, State}.

terminate(_Reason, State) ->
  io:format("Call Finished.~n"),
  ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
