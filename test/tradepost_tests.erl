%%% File    : tradepost_tests.erl
%%% Description : Tradepost test module

-module(tradepost_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("femto_test/include/eunit_fsm.hrl").

-import(tradepost, [which_tp/0, start_tp/0, stop_tp/0,
		    seller_identify_tp/1, seller_insertitem_tp/2, withdraw_item_tp/1]).


pos_start_stop_start_stop_test_() ->
    {inorder,
     [ ?_assertMatch(true,start_tp()),
       ?_assertMatch(ok,stop_tp()),
       ?_assertMatch(true,start_tp()),
       ?_assertMatch(ok,stop_tp())
      ]}.

neg_stop_test_() -> ?_assertError(badarg,stop_tp()).

neg_startTwice_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> stop_tp() end,
     [
      ?_assertMatch(true, start_tp()),
      ?_assertError(badarg, start_tp())
     ]
    }.

pos_start_identify_stop_test_()->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     ?_assertMatch(ok, seller_identify_tp(seller_password))
    }.

pos_start_identify_insert_test_()->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     [
      ?_assertMatch(ok, seller_identify_tp(seller_password)),
      ?_assertMatch(ok, seller_insertitem_tp(playstation,seller_password))
     ]}.

neg_start_insertitem_test_() ->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     ?_assertExit(error,
      		   case seller_insertitem_tp(playstation,seller_password) of
      		       error -> exit(error); Other -> Other end)
    }.

neg_start_withdraw_test_() ->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     ?_assertExit(error,
		  case withdraw_item_tp(seller_password) of
		      error -> exit(error); Other -> Other end)
    }.

neg_identify_withdraw_test_()->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     [
      ?_assertMatch(ok, seller_identify_tp(seller_password)),
      ?_assertError(value,
		    case withdraw_item_tp(seller_password) of
			error ->  error(value); Other -> Other end)
     ]}.

pos_identify_insert_withdraw_twice_test_()->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     [
      ?_assertMatch(ok, seller_identify_tp(seller_password)),
      ?_assertMatch(ok, seller_insertitem_tp(playstation,seller_password)),
      ?_assertMatch(ok, withdraw_item_tp(seller_password)),
      ?_assertMatch(ok, seller_insertitem_tp(playstation,seller_password)),
      ?_assertMatch(ok, withdraw_item_tp(seller_password))
     ]}.

