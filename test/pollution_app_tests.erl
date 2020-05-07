%%%-------------------------------------------------------------------
%% @doc pollution application tests.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_app_tests).
-author("Samuel Heldak").

-include_lib("eunit/include/eunit.hrl").


%% auxiliary function used in most tests
test(Function) ->
  {setup,
    fun() -> pollution_app:start([], []) end,
    fun(_) -> pollution_app:stop([]) end,
    ?_test(Function())}.


%% checking if getOneValue function is returning proper value ({ok, Value})
%% or error({error, no_station} or {error, no_value})
getOneValue() ->
  pollution_gen_server:addStation("Krakow", {1, 1}),
  pollution_gen_server:addStation("Katowice", {2, 2}),
  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15),
  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3),
  Value = pollution_gen_server:getOneValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5"),
  ErrorNoStation = pollution_gen_server:getOneValue({3, 3}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5"),
  ErrorNoValue = pollution_gen_server:getOneValue({2, 2}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5"),

  [?_assertEqual(Value, {ok, 45}),
    ?_assertEqual(ErrorNoStation, {error, no_station}),
    ?_assertEqual(ErrorNoValue, {error, no_value})].

getOneValue_test_() ->
  test(fun() -> getOneValue() end).


%% checking if getStationMean function is returning proper value ({ok, Mean}) or error ({error, no_station})
getStationMean() ->
  pollution_gen_server:addStation("Krakow", {1, 1}),
  pollution_gen_server:addStation("Katowice", {2, 2}),
  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15),
  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3),
  Mean = pollution_gen_server:getStationMean({1, 1}, "PM2.5"),
  ErrorNoStation = pollution_gen_server:getStationMean("Warszawa", "PM2.5"),

  [?_assertEqual(Mean, {ok, 30}),
    ?_assertEqual(ErrorNoStation, {error, no_station})].


getStationMean_test_() ->
  test(fun() -> getStationMean() end).


%% checking if getDailyMean function is returning proper value ({ok, Mean})
getDailyMean() ->
  pollution_gen_server:addStation("Krakow", {1, 1}),
  pollution_gen_server:addStation("Katowice", {2, 2}),
  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15),
  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "PM2.5", 6),
  Mean = pollution_gen_server:getDailyMean({2020, 04, 02}, "PM2.5"),

  ?_assertEqual(Mean, {ok, 22}).

getDailyMean_test_() ->
  test(fun() -> getDailyMean() end).


%% checking if getStationWithTheMostMeasurements function is returning proper value ({ok, {X, Y}})
%% or error ({error, empty_monitor})
getStationWithTheMostMeasurements() ->
  ErrorEmptyMonitor = pollution_gen_server:getStationWithTheMostMeasurements(),
  pollution_gen_server:addStation("Krakow", {1, 1}),
  pollution_gen_server:addStation("Katowice", {2, 2}),
  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15),
  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "PM2.5", 4),
  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {18, 00, 00}}, "PM2.5", 8),
  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {19, 00, 00}}, "PM2.5", 16),
  Station = pollution_gen_server:getStationWithTheMostMeasurements(),

  [?_assertEqual(ErrorEmptyMonitor, {error, empty_monitor}),
    ?_assertEqual(Station, {ok, {2, 2}})].

getStationWithTheMostMeasurements_test_() ->
  test(fun() -> getStationWithTheMostMeasurements() end).


%% checking if getNearestStation function is returning proper coordinates ({ok, {X, Y}})
%% or error ({error, no_station} or {error, just_one_station})
getNearestStation() ->
  pollution_gen_server:addStation("Krakow", {1, 1}),
  ErrorNoStation = pollution_gen_server:getNearestStation({2, 2}),
  ErrorJustOneStation = pollution_gen_server:getNearestStation({1, 1}),

  pollution_gen_server:addStation("Katowice", {1, 3}),
  pollution_gen_server:addStation("Warszawa", {3, 2}),
  pollution_gen_server:addStation("Poznan", {3, 3}),
  Station1 = pollution_gen_server:getNearestStation({1, 1}),
  Station2 = pollution_gen_server:getNearestStation({3, 2}),
  Station3 = pollution_gen_server:getNearestStation({3, 3}),

  [?_assertEqual(ErrorNoStation, {error, no_station}),
    ?_assertEqual(ErrorJustOneStation, {error, just_one_station}),
    ?_assertEqual(Station1, {ok, {1, 3}}),
    ?_assertEqual(Station2, {ok, {3, 3}}),
    ?_assertEqual(Station3, {ok, {3, 2}})].

getNearestStation_test_() ->
  test(fun() -> getNearestStation() end).
