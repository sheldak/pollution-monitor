defmodule PollutionDataStream do
  @moduledoc """
  Loading data from file to the pollution server using Stream.
  """

  @doc """
  Gets lines from CSV file as stream.

  Returns stream.
  """
  def import_lines_from_CSV do
    File.stream!("pollution.csv")
  end

  @doc """
  Makes map containing information about 1 measurement from given `line`.

  Returns map with three items: `:datetime`, `:location`, `:pollution_level`.
  """
  def parse_line(line) do
    [date_str, time_str, x_str, y_str, value_str] = String.split(line, ",")

    date = date_str
           |> String.split("-")
           |> Enum.reverse()
           |> Enum.map(&Integer.parse/1)
           |> Enum.map(&(elem(&1, 0)))
           |> List.to_tuple()

    time = time_str
           |> String.split(":")
           |> Enum.map(&Integer.parse/1)
           |> Enum.map(&(elem(&1, 0)))
           |> List.to_tuple()

    datetime = {date, time}

    location = [x_str, y_str]
               |> Enum.map(&Float.parse/1)
               |> Enum.map(&(elem(&1, 0)))
               |> List.to_tuple()

    pollution_level = elem(Integer.parse(value_str), 0)

    %{:datetime => datetime, :location => location, :pollution_level => pollution_level}
  end

  @doc """
  Extracts all unique stations from `stream`.

  Returns stream of tuples {x, y} where x and y are coordinates of a station.
  """
  def get_stations(stream) do
    stations_coords = stream
                      |> Stream.flat_map(&(String.split(&1, ",")))
                      |> Stream.map(&Float.parse/1)
                      |> Stream.filter(&(elem(&1, 1) == ""))
                      |> Stream.map(fn {x, _} -> x end)

    first_coordinate = Stream.take_every(stations_coords, 2)
    second_coordinate = Stream.drop_every(stations_coords, 2)

    stations = Stream.zip(first_coordinate, second_coordinate)
               |> Stream.uniq()

    stations
  end

  @doc """
  Makes name of station from given `station_location`.

  Returns string which is a name of the station.
  """
  def generate_station_name(station_location) do
    "station_#{elem(station_location, 0)}_#{elem(station_location, 1)}"
  end

  @doc """
  Adds given `stations` (stream of tuples representing locations) to the pollution server.
  """
  def add_stations(stations) do
    add_station_fn = fn station -> :pollution_gen_server.addStation(generate_station_name(station), station) end

    Enum.each(stations, add_station_fn)
  end

  @doc """
  Adds given `measurements` (stream of maps) to the pollution server.
  """
  def add_measurements(measurements) do
    add_measurement_fn = fn measurement -> :pollution_gen_server.
                           addValue(measurement.location, measurement.datetime, "PM10", measurement.pollution_level) end

    Enum.each(measurements, add_measurement_fn)
  end

  @doc """
  Main function which gets stream from the file and saves all measurements to the pollution server.
  Function prints time needed to load stations and measurements to the pollution server.
  """
  def add_measurements_from_file do
    stream = import_lines_from_CSV()
    stations = get_stations stream
    measurements = Stream.map(stream, &parse_line/1)

    :pollution_sup.start_link()
    add_stations_time = fn -> add_stations(stations) end
                        |> :timer.tc
                        |> elem(0)
                        |> Kernel./(1_000_000)

    add_measurements_time = fn -> add_measurements(measurements) end
                            |> :timer.tc
                            |> elem(0)
                            |> Kernel./(1_000_000)

    :timer.sleep(200);
    IO.puts "Time of adding stations: #{add_stations_time}"
    IO.puts "Time of adding measurements: #{add_measurements_time}"
  end
end
