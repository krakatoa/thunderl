-module(thunderl).

-export([connect/3]).

%% {ok, Client} = thunderl:connect("usera@fs.thunderl.com", "1", "192.168.1.106").

%% --> Make a call <--
%% Call = thunderl_registry:get_pid(<<"the-uuid-of-the-call">>),
%% thunderl_call:answer(Call).

connect(UserUrl, Password, Server) ->
  thunderl_client:connect(UserUrl, Password, Server).
