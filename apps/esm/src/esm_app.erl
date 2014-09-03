-module(esm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% ESM (Evolved Session Management)

start(_StartType, _StartArgs) ->
    esm_sup:start_link().

stop(_State) ->
    ok.
