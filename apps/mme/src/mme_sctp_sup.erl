%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2014 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(mme_sctp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->

    MmeSctp = ?CHILD(mme_sctp, worker),
    {ok, { {one_for_one, 5, 10}, [MmeSctp]} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================



