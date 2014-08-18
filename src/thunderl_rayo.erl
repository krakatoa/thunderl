-module(thunderl_rayo).

-export([answer/3]).
-export([hangup/3]).

-export([output/4]).

-include_lib("erim/include/exmpp_xml.hrl").

answer(From, To, Id) ->
  #xmlel{
    ns = "urn:xmpp:rayo:1",
    name='iq',
    attrs=[ #xmlattr{name = <<"from">>, value = From},
            #xmlattr{name = <<"to">>, value = To},
            #xmlattr{name = <<"type">>, value = <<"set">>},
            #xmlattr{name = <<"id">>, value = Id} ],
    children = [ #xmlel{name = 'answer', ns = 'urn:xmpp:rayo:1'} ]
  }.

hangup(From, To, Id) ->
  #xmlel{
    ns = "urn:xmpp:rayo:1",
    name='iq',
    attrs=[ #xmlattr{name = <<"from">>, value = From},
            #xmlattr{name = <<"to">>, value = To},
            #xmlattr{name = <<"type">>, value = <<"set">>},
            #xmlattr{name = <<"id">>, value = Id} ],
    children = [ #xmlel{name = 'hangup', ns = 'urn:xmpp:rayo:1'} ]
  }.

output(From, To, Id, PlaybackUrl) ->
  {xmlel,undefined,[],iq,
        [{xmlattr,undefined,<<"type">>,<<"set">>},
         {xmlattr,undefined,<<"to">>,To},
         {xmlattr,undefined,<<"id">>,Id}],
        [{xmlel,'urn:xmpp:rayo:output:1',
                [{'urn:xmpp:rayo:output:1',none}],
                output,[],
                [thunderl_ssml:playback_node(PlaybackUrl)]}]}.
