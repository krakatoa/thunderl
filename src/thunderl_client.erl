-module(thunderl_client).
-behaviour(gen_server).

-export([connect/3]).
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

  {ok, [Session]}.

go_online(Session) ->
  exmpp_session:send_packet(Session,
            exmpp_presence:set_status(
        exmpp_presence:available(), "Echo Ready")).

handle_call({answer_call, UUID}, From, [Session]) ->
  %% -- TODO abstract this into a command_call --
  Id = <<"iq-id-2">>, %% use uuids!
  AnswerStanza = thunderl_rayo:answer(<<"usera@fs.thunderl.com">>, UUID, Id),
  exmpp_session:send_packet(Session, AnswerStanza),
  thunderl_registry:store_command(answer, Id, From),
  %% {reply, ok, [Session]};
  {noreply, [Session]};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Record = #received_packet{packet_type=presence,type_attr=Type}, State) when Type == "available" ->
  io:format("=== thunderl_client RECV presence [type=available]~n"),
  RawPacket = Record#received_packet.raw_packet,
  UUID = (exmpp_xml:get_attribute_node_from_list(RawPacket#xmlel.attrs, <<"from">>))#xmlattr.value,

  [FirstChild|[SecondChild|_Rest]] = RawPacket#xmlel.children,
  io:format("DEBUG FirstChild.name ~p~n", [FirstChild#xmlel.name]),
  io:format("DEBUG SecondChild.name ~p~n", [SecondChild#xmlel.name]),

  %% [_Auth, Data, _Stamp] = RawPacket#xmlel.children,
  %% io:format("NEW STANZA ~p~n", [RawPacket]),
  case FirstChild#xmlel.name of
    c ->
      case SecondChild#xmlel.name of
        "offer" -> process_offer({UUID, SecondChild});
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

process_offer({UUID, Data}) ->
  From = exmpp_xml:get_attribute_node_from_list(Data#xmlel.attrs, <<"from">>),
  To = exmpp_xml:get_attribute_node_from_list(Data#xmlel.attrs, <<"to">>),
  Children = exmpp_xml:get_child_elements(Data),

  {ok, Call} = thunderl_call:create({UUID, From, To}, Children, self()),
  thunderl_registry:add(UUID, Call).

process_hangup({UUID, _Data}) ->
  Call = thunderl_registry:get_pid(UUID),
  gen_server:cast(Call, {finish}),
  thunderl_registry:delete(UUID).

process_answer({UUID}) ->
  io:format("=== thunderl_client ANSWERED~n").

process_command_response(Id) ->
  From = thunderl_registry:get_command_from(Id),
  gen_server:reply(From, delayed_ok),
  thunderl_registry:delete_command(Id).

terminate(_Reason, State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
