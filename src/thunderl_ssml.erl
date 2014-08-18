-module(thunderl_ssml).

-export([playback_node/1]).

playback_node(Url) ->
  SpeakNode = <<"<speak xmlns=\"http://www.w3.org/2001/10/synthesis\" version=\"1.0\" xml:lang=\"en-US\"><audio src=\"", Url, "\"/></speak>">>,
  {xmlel,'urn:xmpp:rayo:output:1',[],document,
          [{xmlattr,undefined,<<"content-type">>,<<"application/ssml+xml">>}],
          [{xmlcdata,SpeakNode}]}.
