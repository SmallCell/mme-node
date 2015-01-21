-module(mme_node).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok.

start(_StartType, _StartArgs) ->
    mme_node_sup:start_link().

stop(_State) ->
    ok.
