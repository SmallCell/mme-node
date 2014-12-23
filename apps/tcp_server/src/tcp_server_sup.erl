
-module(tcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ListenPort, Module) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, Module]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, Module]) ->
    %% TcpListener = ?CHILD(tcp_listener, worker, [Port, Module]),
    %% TcpClientSup = ?CHILD(tcp_client_sup, supervisor, [Module]),
    {ok, { {one_for_one, ?MAX_RESTART, ?MAX_TIME},
           [  ]} }.

