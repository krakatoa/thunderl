-module(thunderl_call_command).

-export([answer/1, hangup/1]).

answer(CallUUID) ->
  Id = <<"iq-id-2">>, %% use uuids!
  {Id, thunderl_rayo:answer(<<"usera@fs.thunderl.com">>, CallUUID, Id)}.

hangup(CallUUID) ->
  Id = <<"iq-id-2">>, %% use uuids!
  {Id, thunderl_rayo:hangup(<<"usera@fs.thunderl.com">>, CallUUID, Id)}.
