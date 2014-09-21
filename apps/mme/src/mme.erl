%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2014 by vlad <lib.aca55a@gmail.com>

-module(mme).

%% Our Erlang application.
-define(APPLICATION, mme).


%% start/stop explicitly.
-export([start/0, start/2,
         stop/0, stop/1]).

-export_type([]).


%% ---------------------------------------------------------------------------
%% start/0
%% ---------------------------------------------------------------------------
start() ->
    application:start(?APPLICATION).

%% ---------------------------------------------------------------------------
%% stop/0
%% ---------------------------------------------------------------------------

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?APPLICATION).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mme_sup:start_link().

stop(_State) ->
    ok.
