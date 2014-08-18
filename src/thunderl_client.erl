-module(thunderl_client).
-behaviour(gen_server).

-export([connect/3, add_listener/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("erim/include/exmpp_client.hrl").
-include_lib("erim/include/exmpp_xml.hrl").

-include("include/thunderl_call.hrl").

connect(UserUrl, Password, Server) ->
  gen_server:start_link(?MODULE, [UserUrl, Password, Server], []).

init([UserUrl, Password, Server]) ->
  application:start(exmpp),
  [Username|DomainName] = string:tokens(UserUrl, "@"),
  Session = exmpp_session:start_link({1, 0}),
  JID = exmpp_jid:make(Username, DomainName, random),
  io:format("JID: ~p~n", [JID]),
  exmpp_session:auth_info(Session, JID, Password),
  {ok, StreamId, _Features} = exmpp_session:connect_TCP(Session, Server, 5222),
  io:format("Stream: ~p~n", [StreamId]),

  {ok, _JID} = exmpp_session:login(Session, "PLAIN"),
  
  go_online(Session),

  {ok, Events} = gen_event:start_link(),
  

  {ok, {Session, Events}}.

go_online(Session) ->
  exmpp_session:send_packet(Session,
            exmpp_presence:set_status(
        exmpp_presence:available(), "Echo Ready")).

add_listener(Client, Pid) ->
  gen_server:call(Client, {add_event_listener, Pid}).

run_command(From, {Kind, Id, Stanza}, Session) ->
  exmpp_session:send_packet(Session, Stanza),
  thunderl_registry:store_command(Kind, Id, From).

handle_call({answer_call, UUID}, From, State={Session, _Events}) ->
  {Id, Stanza} = thunderl_call_command:answer(UUID),
  run_command(From, {answer, Id, Stanza}, Session),
  %% {reply, ok, [Session]};
  {noreply, State};
handle_call({hangup_call, UUID}, From, State={Session, _Events}) ->
  {Id, Stanza} = thunderl_call_command:hangup(UUID),
  run_command(From, {hangup, Id, Stanza}, Session),
  %% {reply, ok, [Session]};
  {noreply, State};
handle_call({play_call, UUID}, From, State={Session, _Events}) ->
  {Id, Stanza} = thunderl_call_component:play(UUID),
  run_command(From, {play, Id, Stanza}, Session),
  %% {reply, ok, [Session]};
  {noreply, State};
handle_call({add_event_listener, SubscriberPid}, _From, State={_Session, Events}) ->
  FeedHandler = {thunderl_client_events, make_ref()},
  gen_event:add_handler(Events, FeedHandler, [SubscriberPid]),
  {reply, FeedHandler, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Record = #received_packet{packet_type=presence,type_attr=Type}, State={_Session, Events}) when Type == "available" ->
  io:format("=== thunderl_client RECV presence [type=available]~n"),
  RawPacket = Record#received_packet.raw_packet,
  UUID = (exmpp_xml:get_attribute_node_from_list(RawPacket#xmlel.attrs, <<"from">>))#xmlattr.value,

  [FirstChild|[SecondChild|_Rest]] = RawPacket#xmlel.children,
  io:format("DEBUG FirstChild.name ~p~n", [FirstChild#xmlel.name]),
  io:format("DEBUG SecondChild.name ~p~n", [SecondChild#xmlel.name]),

  %% [_Auth, Data, _Stamp] = RawPacket#xmlel.children,
  %% io:format("NEW STANZA ~p~n", [RawPacket]),
  io:format("@handle_info Events=~p~n", [Events]),

  case FirstChild#xmlel.name of
    c ->
      case SecondChild#xmlel.name of
        "offer" -> process_offer({UUID, SecondChild}, Events);
        _ -> io:format("Unhandled xmlel.name=~p~n", [SecondChild#xmlel.name])
      end;
    "answered" -> process_answer({UUID});
    %% "result" -> 
    _ -> io:format("Unhandled xmlel.name=~p~n", [FirstChild#xmlel.name])
  end,
  {noreply, State};
handle_info(Record = #received_packet{packet_type=presence,type_attr=Type}, State) when Type == "unavailable" ->
  io:format("=== thunderl_client RECV presence [type=unavailable]~n"),
  RawPacket = Record#received_packet.raw_packet,
  UUID = (exmpp_xml:get_attribute_node_from_list(RawPacket#xmlel.attrs, <<"from">>))#xmlattr.value,
  [Data, _Stamp] = RawPacket#xmlel.children,
  %% io:format("NEW STANZA ~p~n", [RawPacket]),
  case Data#xmlel.name of
    "end" -> process_hangup({UUID, Data});
    _ -> io:format("Unhandled xmlel.name=~p~n", [Data#xmlel.name])
  end,
  {noreply, State};
handle_info(Record = #received_packet{packet_type=presence,type_attr=Type}, State) when Type =/= "error" ->
  io:format("=== thunderl_client RECV unknown presence stanza [~p]~n", [Record#received_packet.raw_packet]),
  {noreply, State};
handle_info(Record = #received_packet{packet_type=message,type_attr=Type}, State) when Type =/= "error" ->
  io:format("=== thunderl_client RECV unknown message stanza [~p]~n", [Record#received_packet.raw_packet]),
  {noreply, State};
handle_info(Record = #received_packet{packet_type=iq}, State) ->
  RawPacket = Record#received_packet.raw_packet,
  Type = (exmpp_xml:get_attribute_node_from_list(RawPacket#xmlel.attrs, <<"type">>))#xmlattr.value,
  Id = (exmpp_xml:get_attribute_node_from_list(RawPacket#xmlel.attrs, <<"id">>))#xmlattr.value,

  io:format("=== thunderl_client RECV iq [type=result]~n"),
  case Type of
    <<"result">> -> process_command_response(Id);
    _ -> io:format("Unknown iq result stanza~n")
  end,

  {noreply, State};
handle_info(Info, State) ->
  io:format("Unhandled message!~n~p", [Info]),
  {noreply, State}.

process_offer({UUID, Data}, Events) ->
  From = exmpp_xml:get_attribute_node_from_list(Data#xmlel.attrs, <<"from">>),
  To = exmpp_xml:get_attribute_node_from_list(Data#xmlel.attrs, <<"to">>),
  Children = exmpp_xml:get_child_elements(Data),

  {ok, Call} = thunderl_call:create({UUID, From, To}, Children, self()),
  io:format("gen_event:notify: ~p~n", [Events]),
  gen_event:notify(Events, {new_call, Call}),
  thunderl_registry:add(UUID, Call).

process_hangup({UUID, _Data}) ->
  Call = thunderl_registry:get_pid(UUID),
  gen_server:cast(Call, {finish}),
  thunderl_registry:delete(UUID).

process_answer({_UUID}) ->
  io:format("=== thunderl_client ANSWERED~n").

process_command_response(Id) ->
  From = thunderl_registry:get_command_from(Id),
  gen_server:reply(From, delayed_ok),
  thunderl_registry:delete_command(Id).

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
