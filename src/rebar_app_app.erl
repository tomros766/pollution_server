%%%-------------------------------------------------------------------
%% @doc rebar_app public API
%% @end
%%%-------------------------------------------------------------------

-module(rebar_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rebar_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
