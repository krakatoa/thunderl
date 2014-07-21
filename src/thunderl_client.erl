-module(thunderl_client).
-behaviour(gen_server).

-export([connect/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("erim/include/exmpp_client.hrl").
-include_lib("erim/include/exmpp_xml.hrl").

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

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({log, Kind, ServerId, Elapsed}, State) ->
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Record = #received_packet{packet_type=presence}, State) ->
  RawPacket = Record#received_packet.raw_packet,
  [_Auth, Data, _Stamp] = RawPacket#xmlel.children,
  %% io:format("Received Presence Stanza: ~n~p~n", [Record#received_packet.raw_packet]),
  case Data#xmlel.name of
    "offer" -> process_offer({Data});
    _ -> io:format("Unhandled xmlel.name=~p~n", [Data#xmlel.name])
  end,
  {noreply, State};
handle_info(Record = #received_packet{packet_type=message, type_attr=Type}, State) when Type =/= "error" ->
  io:format("Received Message Stanza: ~n~p~n", [Record#received_packet.raw_packet]),
  {noreply, State};
handle_info(Info, State) ->
  io:format("Unhandled message!~n~p", [Info]),
  {noreply, State}.

process_offer({Data}) ->
  From = exmpp_xml:get_attribute_node_from_list(Data#xmlel.attrs, <<"from">>),
  To = exmpp_xml:get_attribute_node_from_list(Data#xmlel.attrs, <<"to">>),
  Children = exmpp_xml:get_child_elements(Data),
  
  io:format("From: ~p~n", [From]),
  io:format("To: ~p~n", [To]).

terminate(_Reason, State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
