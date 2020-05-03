-module(pollution_gen_server_tests).
%%-author("Samuel Heldak").
%%
%%%% API
%%-export([]).
%%-ifdef(TEST).
%%-include_lib("eunit/include/eunit.hrl").
%%
%%
%%%%%% checking if addStation function is adding station properly (checking if it returns "ok")
%%%%test_add_station1() ->
%%%%  pollution_server:start(),
%%%%  Result = pollution_gen_server:addStation("Krakow", {1, 1}),
%%%%  pollution_server:stop(),
%%%%  Result.
%%%%
%%%%%% checking if addStation function generates error in case of duplicating station
%%%%test_add_station2() ->
%%%%  pollution_server:start(),
%%%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%%%  Result = pollution_gen_server:addStation("Krakow", {2, 2}),
%%%%  pollution_server:stop(),
%%%%  Result.
%%%%
%%%%test_add_station3() ->
%%%%  pollution_server:start(),
%%%%  pollution_gen_server:addStation("Katowice", {1, 1}),
%%%%  Result = pollution_gen_server:addStation("Krakow", {1, 1}),
%%%%  pollution_server:stop(),
%%%%  Result.
%%%%
%%%%
%%%%%% checking if addValue function is adding value properly (checking if it returns "ok")
%%%%test_add_value1() ->
%%%%  pollution_server:start(),
%%%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%%%  Result = pollution_gen_server:addValue({1, 1}, {{2020, 04, 10}, {12, 00, 00}}, "PM10", 15),
%%%%  pollution_server:stop(),
%%%%  Result.
%%%%
%%%%%% checking if addValue function generates error in case of passing non-existing station
%%%%test_add_value2() ->
%%%%  pollution_server:start(),
%%%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%%%  Result = pollution_gen_server:addValue({2, 2}, {{2020, 04, 10}, {12, 00, 00}}, "PM10", 15),
%%%%  pollution_server:stop(),
%%%%  Result.
%%%%
%%%%%% checking if addValue function generates error in case of value duplication
%%%%test_add_value3() ->
%%%%  pollution_server:start(),
%%%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 10}, {12, 00, 00}}, "PM10", 15),
%%%%  Result = pollution_gen_server:addValue({1, 1}, {{2020, 04, 10}, {12, 00, 00}}, "PM10", 15),
%%%%  pollution_server:stop(),
%%%%  Result.
%%%%
%%%%
%%%%%% checking if removeValue function is removing value properly
%%%%test_remove_value1() ->
%%%%  pollution_server:start(),
%%%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%%%  pollution_gen_server:addStation("Katowice", {2, 2}),
%%%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15),
%%%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
%%%%  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3),
%%%%  Result = pollution_gen_server:removeValue("Katowice", {{2020, 04, 02}, {13, 00, 00}}, "temp"),
%%%%  pollution_server:stop(),
%%%%  Result.
%%%%
%%%%%% checking if removeValue function generates error in case of passing non-existing station
%%%%test_remove_value2() ->
%%%%  pollution_server:start(),
%%%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%%%  Result = pollution_gen_server:removeValue("Katowice", {{2020, 04, 02}, {13, 00, 00}}, "temp"),
%%%%  pollution_server:stop(),
%%%%  Result.
%%
%%
%%%% checking if getOneValue function is returning proper value ({ok, Value})
%%test_get_one_value1() ->
%%  pollution_app:start([], []),
%%%%  pollution_gen_server:start(),
%%%%  pollution_gen_server:init([]),
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  pollution_gen_server:addStation("Katowice", {2, 2}),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
%%  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3),
%%  Result = pollution_gen_server:getOneValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5"),
%%%%  pollution_app:stop([]),
%%%%  pollution_app:stop([]),
%%%%  pollution_app:stop([]),
%%%%  pollution_gen_server:stop(),
%%  Result.
%%
%%
%%%% checking if getOneValue function generates error in case of passing non-existing station
%%test_get_one_value2() ->
%%  pollution_app:start([], []),
%%%%  pollution_gen_server:start(),
%%
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  Result = pollution_gen_server:getOneValue({2, 2}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5"),
%%%%  pollution_app:stop([]),
%%%%  pollution_gen_server:stop(),
%%  Result.
%%
%%%% checking if getOneValue function generates error in case of passing non-existing value
%%test_get_one_value3() ->
%%%%  pollution_app:stop([]),
%%  pollution_app:start([], []),
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  pollution_gen_server:addStation("Katowice", {2, 2}),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
%%  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3),
%%  Result = pollution_gen_server:getOneValue({2, 2}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5"),
%%%%  pollution_app:stop([]),
%%  Result.
%%
%%
%%%% checking if getStationMean function is returning proper value ({ok, Mean})
%%test_get_station_mean1() ->
%%  pollution_app:start([], []),
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  pollution_gen_server:addStation("Katowice", {2, 2}),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
%%  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3),
%%  Result = pollution_gen_server:getStationMean({1, 1}, "PM2.5"),
%%  pollution_app:stop([]),
%%  Result.
%%
%%
%%%% checking if getStationMean function generates error in case of passing non-existing station
%%test_get_station_mean2() ->
%%  pollution_app:start([], []),
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  Result = pollution_gen_server:getStationMean("Katowice", "PM2.5"),
%%  pollution_app:stop([]),
%%  Result.
%%
%%
%%%% checking if getDailyMean function is returning proper value
%%test_get_daily_mean() ->
%%  pollution_app:start([], []),
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  pollution_gen_server:addStation("Katowice", {2, 2}),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
%%  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "PM2.5", 6),
%%  Result = pollution_gen_server:getDailyMean({2020, 04, 02}, "PM2.5"),
%%  pollution_app:stop([]),
%%  Result.
%%
%%
%%%% checking if getStationWithTheMostMeasurements function is returning proper value
%%test_get_station_with_the_most_measurements1() ->
%%  pollution_app:start([], []),
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  pollution_gen_server:addStation("Katowice", {2, 2}),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15),
%%  pollution_gen_server:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45),
%%  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "PM2.5", 4),
%%  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {18, 00, 00}}, "PM2.5", 8),
%%  pollution_gen_server:addValue({2, 2}, {{2020, 04, 02}, {19, 00, 00}}, "PM2.5", 16),
%%  Result = pollution_gen_server:getStationWithTheMostMeasurements(),
%%  pollution_app:stop([]),
%%  Result.
%%
%%%% checking if getStationWithTheMostMeasurements function generates error in case of no stations
%%test_get_station_with_the_most_measurements2() ->
%%  pollution_app:start([], []),
%%  Result = pollution_gen_server:getStationWithTheMostMeasurements(),
%%  pollution_app:stop([]),
%%  Result.
%%
%%%% checking if getNearestStation function is returning proper coordinates
%%test_get_nearest_station1({X, Y}) ->
%%  pollution_app:start([], []),
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  pollution_gen_server:addStation("Katowice", {1, 3}),
%%  pollution_gen_server:addStation("Warszawa", {3, 2}),
%%  pollution_gen_server:addStation("Poznan", {3, 3}),
%%  Result = pollution_gen_server:getNearestStation({X, Y}),
%%  pollution_app:stop([]),
%%  Result.
%%
%%
%%%% checking if getNearestStation function generates error in case of just one station or
%%%% passing non-existing station
%%test_get_nearest_station2({X, Y}) ->
%%  pollution_app:start([], []),
%%  pollution_gen_server:addStation("Krakow", {1, 1}),
%%  Result = pollution_gen_server:getNearestStation({X, Y}),
%%  pollution_app:stop([]),
%%  Result.
%%
%%
%%all_test_() ->
%%  [
%%%%    ?_assert(test_add_station1() =:= ok),
%%%%    ?_assert(test_add_station2() =:= {error, duplicated_station}),
%%%%    ?_assert(test_add_station3() =:= {error, duplicated_station}),
%%%%
%%%%    ?_assert(test_add_value1() =:= ok),
%%%%    ?_assert(test_add_value2() =:= {error, no_station}),
%%%%    ?_assert(test_add_value3() =:= {error, duplicated_value}),
%%%%
%%%%    ?_assert(test_remove_value1() =:= ok),
%%%%    ?_assert(test_remove_value2() =:= {error, no_station}),
%%
%%    ?_assert(test_get_one_value1() =:= 45),
%%    ?_assert(test_get_one_value2() =:= {error, no_station})
%%%%    ?_assert(test_get_one_value3() =:= {error, no_value}),
%%%%
%%%%    ?_assert(test_get_station_mean1() == 30),
%%%%    ?_assert(test_get_station_mean2() =:= {error, no_station}),
%%%%
%%%%    ?_assert(test_get_daily_mean() == 22),
%%%%
%%%%    ?_assert(test_get_station_with_the_most_measurements1() == {2, 2}),
%%%%    ?_assert(test_get_station_with_the_most_measurements2() =:= {error, empty_monitor}),
%%%%
%%%%    ?_assert(test_get_nearest_station1({1, 1}) =:= {1, 3}),
%%%%    ?_assert(test_get_nearest_station1({3, 2}) =:= {3, 3}),
%%%%    ?_assert(test_get_nearest_station1({3, 3}) =:= {3, 2}),
%%%%    ?_assert(test_get_nearest_station2({1, 1}) =:= {error, just_one_station}),
%%%%    ?_assert(test_get_nearest_station2({2, 2}) =:= {error, no_station})
%%  ].
%%
%%
%%-endif.