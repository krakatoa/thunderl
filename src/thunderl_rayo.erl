-module(thunderl_rayo).

-export([answer/3]).

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
