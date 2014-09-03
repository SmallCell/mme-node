-module(emm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% The EPS Mobility Management (EMM) states describe the Mobility
%% Management states that result from the mobility management procedures
%% e.g. Attach and Tracking Area Update procedures.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    emm_sup:start_link().

stop(_State) ->
    ok.
