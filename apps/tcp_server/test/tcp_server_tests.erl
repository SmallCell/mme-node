%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 19 Dec 2014 by vlad <lib.aca55a@gmail.com>

-module(tcp_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_fsm/include/eunit_seq_trace.hrl").

-define(DEF_PORT,    2222).

tcp_server_setup_test_() ->
   {timeout, 3000,
     {setup,
      fun setup/0,
      fun teardown/1, 
         [{"Start/stop LINC common logic", fun logic/0}]}}.
 
 logic() -> 
    %% SPAWN TRACES
    Pid = spawn(eunit_seq_trace,tracer,[]),
    seq_trace:set_system_tracer(Pid), % set Pid as the system tracer  
 
    ?assertEqual(ok, application:start(tcp_server)), 
    {ok,S} = gen_tcp:connect({127,0,0,1},?DEF_PORT,[{packet,2}]),    
    ok = gen_tcp:send(S,"hello"), 
    receive {tcp, _, M} ->     
            ?assertEqual("hello", M)
    end. 
   
setup() ->
    erlang:display(setup),
    ensure_started(sasl),
      ok.   
 
teardown(_) ->
    application:stop(tcp_server),
    ok.


ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
