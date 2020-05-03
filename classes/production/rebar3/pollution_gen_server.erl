-module(pollution_gen_server).

-behavior(gen_server).

%% API
-export([start/1, init/1, stop/0, handle_call/3, handle_cast/2, terminate/2]).
-export([crash/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2,
  getStationWithTheMostMeasurements/0, getNearestStation/1]).

start(InitialMonitor) ->
  gen_server:start_link({local, pollution}, ?MODULE, InitialMonitor, []).


init(InitialMonitor) ->
  io:format("~n=================  Server init  ==================~n~n"),
  {ok, InitialMonitor}.

stop() ->
  gen_server:cast(pollution, {stop}).


%% ----- USER INTERFACE -----

% for crash test
crash() ->
  Y = 0,
  X = 1 / Y,
  X.


%% adding new station
addStation(Name, Coordinates) ->
  gen_server:cast(pollution, {addStation, Name, Coordinates}).


%% adding new measurement to the station
addValue(Name, Date, Type, Value) ->
  gen_server:cast(pollution, {addValue, Name, Date, Type, Value}).


%% removing a measurement
removeValue(Name, Date, Type) ->
  gen_server:cast(pollution, {removeValue, Name, Date, Type}).


%% getting value of the measurement
getOneValue(Name, Date, Type) ->
  gen_server:call(pollution, {getOneValue, Name, Date, Type}).


%% getting mean value of all the measurements at the station
getStationMean(Name, Type) ->
  gen_server:call(pollution, {getStationMean, Name, Type}).


%% getting daily mean value of measurements of the given Type from all stations
getDailyMean(Day, Type) ->
  gen_server:call(pollution, {getDailyMean, Day, Type}).


%% getting coordinates ({X, Y}) of the station which has the highest number of measurements
getStationWithTheMostMeasurements() ->
  gen_server:call(pollution, {getStationWithTheMostMeasurements}).


%% function returning coordinates of the station which are the closest to the given one
getNearestStation(Coordinates) ->
  gen_server:call(pollution, {getNearestStation, Coordinates}).



%% ----- CALLBACKS -----
handle_cast({stop}, Monitor) ->
  {stop, terminated, Monitor};

handle_cast({addStation, Name, Coordinates}, Monitor) ->
  case pollution:addStation(Name, Coordinates, Monitor) of
    {error, _} ->
      io:format("Cannot add station. Duplicated station's name or coordinates.~n", []),
      {noreply, Monitor};
    NewMonitor ->
      io:format("Station ~s added.~n", [Name]),
      {noreply, NewMonitor}
  end;

handle_cast({addValue, Name, Date, Type, Value}, Monitor) ->
  case pollution:addValue(Name, Date, Type, Value, Monitor) of
    {error, no_station} ->
      io:format("Cannot add value. Cannot find station with that name.~n"),
      {noreply, Monitor};
    {error, duplicated_value} ->
      io:format("Cannot add value. Duplicated measurement.~n"),
      {noreply, Monitor};
    NewMonitor ->
      io:format("Measurement ~s = ~B added to the station.~n", [Type, Value]),
      {noreply, NewMonitor}
  end;

handle_cast({removeValue, Name, Date, Type}, Monitor) ->
  case pollution:removeValue(Name, Date, Type, Monitor) of
    {error, no_station} ->
      io:format("Cannot remove value. Cannot find station with that name.~n"),
      {noreply, Monitor};
    NewMonitor ->
      io:format("Measurement removed from the station.~n"),
      {noreply, NewMonitor}
  end.


handle_call({getOneValue, Name, Date, Type}, _, Monitor) ->
  case pollution:getOneValue(Name, Date, Type, Monitor) of
    {error, no_station} ->
      io:format("Cannot get value. Cannot find station with that name.~n"),
      {reply, {error, no_station}, Monitor};
    {error, no_value} ->
      io:format("Cannot get value. There is no such measurement.~n"),
      {reply, {error, no_value}, Monitor};
    Value ->
      io:format("The result of the measurement is ~w.~n", [Value]),
      {reply, Value, Monitor}
  end;

handle_call({getStationMean, Name, Type}, _, Monitor) ->
  case pollution:getStationMean(Name, Type, Monitor) of
    {error, no_station} ->
      io:format("Cannot remove value. Cannot find station with that name.~n"),
      {reply, {error, no_station}, Monitor};
    Mean ->
      io:format("Station mean is equal to ~w.~n", [Mean]),
      {reply, Mean, Monitor}
  end;

handle_call({getDailyMean, Day, Type}, _, Monitor) ->
  Mean = pollution:getDailyMean(Day, Type, Monitor),
  io:format("Daily mean is equal to ~w.~n", [Mean]),
  {reply, Mean, Monitor};

handle_call({getStationWithTheMostMeasurements}, _, Monitor) ->
  case pollution:getStationWithTheMostMeasurements(Monitor) of
    {error, empty_monitor} ->
      io:format("Cannot get the station. Empty monitor.~n"),
      {reply, {error, empty_monitor}, Monitor};
    {X, Y} ->
      io:format("The most measurements has the station (~w,~w).~n", [X, Y]),
      {reply, {X, Y}, Monitor}
  end;

handle_call({getNearestStation, Coordinates}, _, Monitor) ->
  case pollution:getNearestStation(Coordinates, Monitor) of
    {error, no_station} ->
      io:format("There is no station with such coordinates.~n", []),
      {reply, {error, no_station}, Monitor};
    {error, just_one_station} ->
      io:format("Cannot find the closest station. There is just one in the monitor.~n", []),
      {reply, {error, just_one_station}, Monitor};
    {X, Y} ->
      io:format("The closest station is (~w,~w).~n", [X, Y]),
      {reply, {X, Y}, Monitor}
  end.


terminate(Reason, Monitor) ->
  io:format("~nServer: exit with state of monitor: ~p~n", [Monitor]),
  io:format("Server: exit with reason: ~p~n~n", [Reason]),
  Reason.