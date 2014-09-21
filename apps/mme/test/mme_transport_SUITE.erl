%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2014 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(mme_transport_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


groups() ->
    [{all, [], tc()}].

tc() ->
    [sctp_connect,
     reconnect].

all() -> 
    [start,
     {group, all},
     {group, all, [parallel]},
     stop].

start(_Config) ->
    ok = mme:start().

stop(_Config) ->
    ok = mme:stop().



%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
%% my_test_case() -> 
%%     [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
%% my_test_case(_Config) -> 
%%     ok.


