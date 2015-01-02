-module(sctp_client_app).

-behaviour(application).

%% Internal API
-export([start_connector/0, 
         start_server_peer/0]).

%% Application callbacks
-export([start/2, stop/1]).


%% A startup function for spawning new client connection.
start_connector() ->
    supervisor:start_child(sctp_connector_sup, []).

%% A startup function for spawning new server connection handling FSM.
%% To be called by the SCTP listener process.
start_server_peer() ->
    supervisor:start_child(sctp_server_peer_sup, []).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sctp_client_sup:start_link(sctp_client_fsm).

stop(_State) ->
    ok.
