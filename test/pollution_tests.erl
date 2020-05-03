%%%-------------------------------------------------------------------
%% @doc pollution functions tests.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_tests).
-author("Samuel Heldak").

-include_lib("eunit/include/eunit.hrl").

%% checking if new monitor is empty
createMonitor_test_() ->
  ?_assert(maps:size(pollution:createMonitor()) =:= 0).


%% checking if addStation function is adding station properly
addStation_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  ?_assert(M2 =:= #{{1, 1} => #{name => "Krakow"}}).

%% checking if addStation function generates error in case of duplicating station
addStation2_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {1, 1}, M2),
  ?_assert(M3 =:= {error, duplicated_station}).

addStation3_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Krakow", {2, 2}, M2),
  ?_assert(M3 =:= {error, duplicated_station}).


%% checking if addValue function is adding value properly
addValue_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M2),
  M4 = pollution:addValue("Krakow", {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 30, M3),
  ?_assert(M4 =:= #{{1, 1} =>
  #{name => "Krakow",
    {{{2020, 04, 02}, {12, 00, 00}}, "PM10"} => 15,
    {{{2020, 04, 02}, {12, 00, 00}}, "PM2.5"} => 30}}).

%% checking if addValue function generates error in case of passing non-existing station
addValue2_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addValue({2, 2}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M2),
  ?_assert(M3 =:= {error, no_station}).

%% checking if addValue function generates error in case of value duplication
addValue3_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M2),
  M4 = pollution:addValue("Krakow", {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M3),
  ?_assert(M4 =:= {error, duplicated_value}).


%% checking if removeValue function is removing value properly
removeValue_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3, M5),
  M7 = pollution:removeValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", M6),
  M8 = pollution:removeValue("Katowice", {{2020, 04, 02}, {13, 00, 00}}, "temp", M7),
  ?_assert(M8 =:=
    #{{1, 1} =>
    #{name => "Krakow",
      {{{2020, 04, 02}, {16, 00, 00}}, "PM2.5"} => 45},
      {2, 2} =>
      #{name => "Katowice"}}).

%% checking if removeValue function generates error in case of passing non-existing station
removeValue2_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:removeValue("Katowice", {{2020, 04, 02}, {13, 00, 00}}, "temp", M2),
  ?_assert(M3 =:= {error, no_station}).


%% checking if getOneValue function is returning proper value
getOneValue_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3, M5),
  M7 = pollution:getOneValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", M6),
  ?_assert(M7 =:= 45).

%% checking if getOneValue function generates error in case of passing non-existing station
getOneValue2_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:getOneValue({2, 2}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", M2),
  ?_assert(M3 =:= {error, no_station}).

%% checking if getOneValue function generates error in case of passing non-existing value
getOneValue3_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM10", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3, M5),
  M7 = pollution:getOneValue({2, 2}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", M6),
  ?_assert(M7 =:= {error, no_value}).


%% checking if getStationMean function is returning proper value
getStationMean_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "temp", 3, M5),
  M7 = pollution:getStationMean({1, 1}, "PM2.5", M6),
  ?_assert(M7 == 30).

%% checking if getStationMean function generates error in case of passing non-existing station
getStationMean2_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:getStationMean("Katowice", "PM2.5", M2),
  ?_assert(M3 =:= {error, no_station}).


%% checking if getDailyMean function is returning proper value
getDailyMean_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "PM2.5", 6, M5),
  M7 = pollution:getDailyMean({2020, 04, 02}, "PM2.5", M6),
  ?_assert(M7 == 22).


%% checking if getStationWithTheMostMeasurements function is returning proper value
getStationWithTheMostMeasurements_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {2, 2}, M2),
  M4 = pollution:addValue({1, 1}, {{2020, 04, 02}, {12, 00, 00}}, "PM2.5", 15, M3),
  M5 = pollution:addValue({1, 1}, {{2020, 04, 02}, {16, 00, 00}}, "PM2.5", 45, M4),
  M6 = pollution:addValue({2, 2}, {{2020, 04, 02}, {13, 00, 00}}, "PM2.5", 4, M5),
  M7 = pollution:addValue({2, 2}, {{2020, 04, 02}, {18, 00, 00}}, "PM2.5", 8, M6),
  M8 = pollution:addValue({2, 2}, {{2020, 04, 02}, {19, 00, 00}}, "PM2.5", 16, M7),
  M9 = pollution:getStationWithTheMostMeasurements(M8),
  ?_assert(M9 =:= {2, 2}).

%% checking if getStationWithTheMostMeasurements function generates error in case of no stations
getStationWithTheMostMeasurements2_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:getStationWithTheMostMeasurements(M1),
  ?_assert(M2 =:= {error, empty_monitor}).


%% checking if getNearestStation function is returning proper coordinates
getNearestStation_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  M3 = pollution:addStation("Katowice", {1, 3}, M2),
  M4 = pollution:addStation("Warszawa", {3, 2}, M3),
  M5 = pollution:addStation("Poznan", {3, 3}, M4),
  Station1 = pollution:getNearestStation({1, 1}, M5),
  Station2 = pollution:getNearestStation({3, 2}, M5),
  Station3 = pollution:getNearestStation({3, 3}, M5),
  [?_assert(Station1 =:= {1, 3}),
  ?_assert(Station2 =:= {3, 3}),
  ?_assert(Station3 =:= {3, 2})].

%% checking if getNearestStation function generates error in case of just one station or
%% passing non-existing station
getNearestStation4_test_() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("Krakow", {1, 1}, M1),
  StationError1 = pollution:getNearestStation({1, 1}, M2),
  StationError2 = pollution:getNearestStation({2, 2}, M2),
  [?_assert(StationError1 =:= {error,just_one_station}),
  ?_assert(StationError2 =:= {error,no_station})].
