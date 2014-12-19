
-module(tcp_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Internal API
-export([start_client/0]).

-define(DEF_PORT,    2222).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(tcp_client_sup, []).
 

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    tcp_server_sup:start_link(ListenPort, tcp_echo_fsm).

stop(_State) ->
    ok.


%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.
