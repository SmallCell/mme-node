%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 19 Dec 2014 by vlad <lib.aca55a@gmail.com>

-module(tcp_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_fsm/include/eunit_seq_trace.hrl").

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

start_stop_test_() ->
    {setup,
     fun() ->
             erlang:display(setup),
             %% ensure_started(sasl),
             application:start(tcp_server)
     end,
     fun(_) ->
             erlang:display(cleanup),
             application:stop(tcp_server)
     end,
     ?_test(
        begin
            receive after 3000 -> ok end
        end)
    }.

