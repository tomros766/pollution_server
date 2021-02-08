%%%-------------------------------------------------------------------
%%% @author tomasz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. maj 2020 18:44
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("tomasz").
-behavior(gen_server).
-import(pollution, [addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getMeanFromArea/3, getMaxPollution/2]).


%% API
-export([start_link/0, init/1, crash/0, terminate/1, addStation/2, addValue/4, removeValue/3, getOneValue/3,
  getStationMean/2, getDailyMean/2, getMeanFromArea/2, getMaxPollution/1, handle_cast/2, handle_call/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).
init(N) ->
  {ok, N}.

crash() -> gen_server:cast(?MODULE, crash).
addStation(Name, Coords) -> gen_server:cast(?MODULE, {addStation, Name, Coords}).
addValue(Key, Date, Type, Value) -> gen_server:cast(?MODULE, {addValue, Key, Date, Type, Value}).
removeValue(Key, Date, Type) -> gen_server:cast(?MODULE, {removeValue, Key, Date, Type}).
getOneValue(Key, Date, Type) -> gen_server:call(?MODULE, {getOneValue, Key, Date, Type}).
getStationMean(Key, Type) -> gen_server:call(?MODULE, {getStationMean, Key, Type}).
getDailyMean(Day, Type) -> gen_server:call(?MODULE, {getDailyMean, Day, Type}).
getMeanFromArea(Type, Border) -> gen_server:call(?MODULE, {getMeanFromArea, Type, Border}).
getMaxPollution(Type) -> gen_server:call(?MODULE, {getMaxPollution, Type}).

handle_cast({addStation, Name, Coords}, M) -> New_M = pollution:addStation(Name, Coords, M), inner_cast(New_M, M);
handle_cast({addValue, Key, Date, Type, Value}, M) -> New_M = pollution:addValue(Key, Date, Type, Value, M), inner_cast(New_M, M);
handle_cast({removeValue, Key, Date, Type}, M) -> New_M = pollution: removeValue(Key, Date, Type, M), inner_cast(New_M, M);
handle_cast(crash, M) -> 2 + badarg, {noreply, M}.

inner_cast({error, Msg}, M) -> io:format("~s~n", [Msg]), {noreply, M};
inner_cast(New_M, _) -> {noreply, New_M}.


handle_call({getOneValue, Key, Date, Type}, _From, M) -> Val =  pollution:getOneValue(Key, Date, Type, M), inner_call(Val, M);
handle_call({getStationMean, Key, Type}, _From, M) -> Val = pollution:getStationMean(Key, Type, M), inner_call(Val, M);
handle_call({getDailyMean, Day, Type}, _From, M) -> Val = pollution:getDailyMean(Day, Type, M), inner_call(Val, M);
handle_call({getMeanFromArea, Type, Border}, _From, M) -> Val = pollution:getMeanFromArea(Type, Border, M), inner_call(Val, M);
handle_call({getMaxPollution, Type}, _From, M) -> Val = pollution:getMaxPollution(Type, M), inner_call(Val, M);

handle_call(terminate, _From, N) -> {stop, normal, ok, N};
handle_call(close, _From, N) -> {stop, normal, ok, N}.

inner_call({error, Msg}, M) -> io:format("~s~n", [Msg]), {reply, unknown, M};
inner_call(Val, M) -> {reply, Val, M}.


terminate(normal) -> io:format("Closing now, Bye!~n"), ok.