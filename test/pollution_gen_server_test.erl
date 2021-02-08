%%%-------------------------------------------------------------------
%%% @author tomasz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. maj 2020 14:12
%%%-------------------------------------------------------------------
-module(pollution_gen_server_test).
-author("tomasz").

-include_lib("eunit/include/eunit.hrl").

getup_test() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:crash(),
  ?assertMatch({error, {already_started, _}}, pollution_gen_server:start_link()).

func_test() ->
  ?assertEqual(ok, pollution_gen_server:addStation("name", {0, 0})).

fail_op_test() ->
  pollution_gen_server:addStation("name", {0,0}),
  ?assertEqual(ok, pollution_gen_server:addStation("name", {0, 1})).
