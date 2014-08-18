-module(thunderl_call_component).

-export([play/2]).

play(CallUUID, Url) ->
  Id = <<"iq-id-3">>, %% use uuids!
  {Id, thunderl_rayo:output(<<"usera@fs.thunderl.com">>, CallUUID, Id, Url)}.
