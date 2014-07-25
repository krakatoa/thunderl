-module(thunderl_registry).
-behaviour(gen_server).

-include("include/thunderl.hrl").

-export([start_link/0, add/2, get/1, get_pid/1, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(UUID, Pid) ->
  io:format("=== thunderl_registry ADD ~p~p~n", [UUID, Pid]),
  ok = gen_server:call(?MODULE, {add, [UUID, Pid]}).

get(UUID) ->
  io:format("=== thunderl_registry GET ~p~n", [UUID]),
  gen_server:call(?MODULE, {get, [UUID]}).

get_pid(UUID) ->
  io:format("=== thunderl_registry GET_PID ~p~n", [UUID]),
  {ok, Call} = gen_server:call(?MODULE, {get, [UUID]}),
  Call#thunderl_call.pid.

delete(UUID) ->
  io:format("=== thunderl_registry DELETE ~p~n", [UUID]),
  ok = gen_server:call(?MODULE, {delete, [UUID]}).

init([]) ->
  io:format("=== thunderl_registry START~n"),
  Registry = orddict:new(),
  {ok, Registry}.

handle_call({add, [UUID, Pid]}, _From, Registry) ->
  io:format("=== thunderl_registry handle_call ADD ~n"),
  NewCall = #thunderl_call{pid=Pid},
  io:format("=== thunderl_registry ADD ~p~n", [NewCall]),
  NewRegistry = orddict:store(UUID, NewCall, Registry),
  {reply, ok, NewRegistry};
handle_call({get, [UUID]}, _From, Registry) ->
  {ok, CallPid} = orddict:find(UUID, Registry),
  {reply, {ok, CallPid}, Registry};
handle_call({delete, [UUID]}, _From, Registry) ->
  NewRegistry = orddict:erase(UUID, Registry),
  {reply, ok, Registry};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  io:format("Unhandled message!~n~p", [Info]),
  {noreply, State}.

terminate(_Reason, State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
