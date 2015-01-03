%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 19 Dec 2014 by vlad <lib.aca55a@gmail.com>

-module(sctp_client_tests).
 
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(DEF_HOST, {127,0,0,1}).
-define(DEF_PORT,    3333).


sctp_client_setup_test_() ->
   {timeout, 3000,
     {setup,
      fun setup/0, 
      fun teardown/1,
         [{"Start/stop SCTP client common logic", fun logic/0}]}}.


logic() ->
    ?assertEqual(ok, application:start(sctp_server)),
    case get_status(whereis(sctp_listener), "State") of
        {state,Port,sctp_echo_fsm} when is_port(Port) -> 
            ?assert(is_port(Port))
    end, 
    ?assertEqual(ok, application:start(sctp_client)),

    {ok, ConnPid} = sctp_client_app:start_connector(),
    case get_status(ConnPid, "State") of
        {state, _,undefined,undefined,sctp_client_fsm} -> ok
    end,
    ok = sctp_connector:connect(ConnPid, ?DEF_HOST, ?DEF_PORT),
    timer:sleep(1000), 
    HndPid = case get_status(ConnPid, "State") of
                 {state,_,_,Val,sctp_client_fsm} -> Val
             end,
    case get_status(HndPid, "StateName") of
        'WAIT_FOR_DATA' -> ok
    end,

    %% ?assertEqual(ok, application:start(sctp_client)),
    %% Ref = sctp_connector:connect(Pid, ?DEF_HOST, ?DEF_PORT),
    timer:sleep(1000), 
    application:stop(sctp_server), 
    ok.  
       
   
setup() ->
    ensure_started(sasl), 
    ensure_started(compiler),
    ensure_started(syntax_tools), 
    ensure_started(lager),
    ok.
    
teardown(_) ->
    application:stop(sctp_client),
    application:stop(sctp_server),
    ok.

get_status(Pid, Which) ->
    {status, Pid, _Mod, List} = sys:get_status(Pid),
    AllData = lists:flatten([ X || {data, X} <- lists:last(List)]),
    proplists:get_value(Which, AllData).

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
