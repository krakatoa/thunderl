-module(thunderl_registry).
-behaviour(gen_server).

-include("include/thunderl.hrl").

-export([start_link/0, add/2, get/1, get_pid/1, delete/1]).
-export([store_command/3, get_command_from/1, delete_command/1]).
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

store_command(Kind, Id, From) ->
  io:format("=== thunderl_registry STORE Command ~p with id ~p~n", [Kind, Id]),
  ok = gen_server:call(?MODULE, {store_command, [Kind, Id, From]}).

get_command_from(Id) ->
  io:format("=== thunderl_registry GET_COMMAND_PID ~p~n", [Id]),
  {ok, Command} = gen_server:call(?MODULE, {get_command, [Id]}),
  Command#thunderl_command.from.

delete_command(Id) ->
  io:format("=== thunderl_registry DELETE Command with id ~p~n", [Id]),
  ok = gen_server:call(?MODULE, {delete_command, [Id]}).

init([]) ->
  io:format("=== thunderl_registry START~n"),
  Registry = #thunderl_registry{calls = orddict:new(), commands = orddict:new()},
  {ok, Registry}.

handle_call({add, [UUID, Pid]}, _From, Registry) ->
  io:format("=== thunderl_registry handle_call ADD ~n"),
  NewCall = #thunderl_call{pid=Pid},
  io:format("=== thunderl_registry ADD_CALL ~p~n", [NewCall]),
  NewCalls = orddict:store(UUID, NewCall, Registry#thunderl_registry.calls),
  {reply, ok, #thunderl_registry{calls = NewCalls, commands = Registry#thunderl_registry.commands}};
handle_call({get, [UUID]}, _From, Registry) ->
  {ok, CallPid} = orddict:find(UUID, Registry#thunderl_registry.calls),
  {reply, {ok, CallPid}, Registry};
handle_call({delete, [UUID]}, _From, Registry) ->
  NewCalls = orddict:erase(UUID, Registry#thunderl_registry.calls),
  {reply, ok, #thunderl_registry{calls = NewCalls, commands = Registry#thunderl_registry.commands}};
handle_call({store_command, [Kind, Id, From]}, _From, Registry) ->
  NewCommand = #thunderl_command{kind=Kind, id=Id, from=From},
  io:format("=== thunderl_registry ADD_COMMAND ~p~n", [NewCommand]),
  NewCommands = orddict:store(Id, NewCommand, Registry#thunderl_registry.commands),
  {reply, ok, #thunderl_registry{commands = NewCommands, calls = Registry#thunderl_registry.calls}};
handle_call({get_command, [Id]}, _From, Registry) ->
  {ok, From} = orddict:find(Id, Registry#thunderl_registry.commands),
  {reply, {ok, From}, Registry};
handle_call({delete_command, [Id]}, _From, Registry) ->
  NewCommands = orddict:erase(Id, Registry#thunderl_registry.commands),
  {reply, ok, #thunderl_registry{commands = NewCommands, calls = Registry#thunderl_registry.calls}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  io:format("Unhandled message!~n~p", [Info]),
  {noreply, State}.

terminate(_Reason, State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
