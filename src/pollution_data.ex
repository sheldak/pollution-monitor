defmodule PollutionData do
  @moduledoc false

  def import_lines_from_CSV do
    lines = File.read!("pollution.csv") |>
            String.split("\r\n")
    lines
  end

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

    measurement = %{:datetime => datetime, :location => location, :pollution_level => pollution_level}

    measurement
  end

  def identify_stations(measurements) do
    stations = measurements
               |> Enum.map(fn measurement -> measurement.location end)
               |> Enum.uniq_by(&Function.identity/1)

    stations
  end

  def generate_station_name(station) do
    name = "station_#{elem(station, 0)}_#{elem(station, 1)}"

    name
  end

  def add_stations(stations) do
    add_station_fn = fn station -> :pollution_gen_server.addStation(generate_station_name(station), station) end

    Enum.each(stations, add_station_fn)
  end

  def add_measurements(measurements) do
    add_measurement_fn = fn measurement -> :pollution_gen_server.
                          addValue(measurement.location, measurement.datetime, "PM10", measurement.pollution_level) end

    Enum.each(measurements, add_measurement_fn)
  end

  def add_measurements_from_file do
    lines = import_lines_from_CSV()
    measurements = Enum.map(lines, &parse_line/1)
    stations = identify_stations(measurements)

    :pollution_sup.start_link()
    add_stations_time = fn -> add_stations(stations) end
                        |> :timer.tc
                        |> elem(0)
                        |> Kernel./(1_000_000)

    add_measurements_time = fn -> add_measurements(measurements) end
                            |> :timer.tc
                            |> elem(0)
                            |> Kernel./(1_000_000)

    example_station = {19.896, 50.077}
    example_day = {2017, 5, 3}

    get_station_mean_time = fn -> :pollution_gen_server.getStationMean(example_station, "PM10") end
                            |> :timer.tc
                            |> elem(0)
                            |> Kernel./(1_000_000)

    get_daily_mean_time = fn -> :pollution_gen_server.getDailyMean(example_day, "PM10") end
                            |> :timer.tc
                            |> elem(0)
                            |> Kernel./(1_000_000)

    :timer.sleep(1000);
    IO.puts "Time of adding stations: #{add_stations_time}"
    IO.puts "Time of adding measurements: #{add_measurements_time}"
    IO.puts "Time of getting station mean: #{get_station_mean_time}"
    IO.puts "Time of getting daily mean: #{get_daily_mean_time}"

  end

end
