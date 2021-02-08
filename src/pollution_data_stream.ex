defmodule PollutionDataStream do
  @moduledoc false
  def importLinesFromCSV(name \\ "pollution.csv") do
    File.stream!(name)
  end

  def convertLine(line) do
    [date, time, longitude, latitude, value] = line |> String.split(",")
    parsedDate = date |> String.split("-") |> Enum.reverse() |> Enum.map( fn(x) -> elem(Integer.parse(x), 0) end) |> :erlang.list_to_tuple()
    parsedTime = time |> String.split(":") |> Enum.map( fn(x) -> elem(Integer.parse(x), 0) end) |> :erlang.list_to_tuple()
    {{x, _}, {y, _}} = {Float.parse(longitude), Float.parse(latitude)}
    location = {x, y}
    {numValue, _} = Float.parse(value)
    %{:datetime => {parsedDate, parsedTime}, :location => location, :pollutionLevel => numValue}
  end

  def identifyStations(mappedLine) do
    mappedLine |> Stream.uniq_by(fn(x) -> x[:location] end)
  end

  def loadStation(line) do
    :pollution_gen_server.addStation("station_#{elem(line[:location], 0)}_#{elem(line[:location], 1)}", line[:location])
  end

  def loadMeasurement(line) do
    :pollution_gen_server.addValue(line[:location], line[:datetime], "PM10", line[:pollutionLevel])
  end

  def test do
    loadingStations = testStations()
    loadingMeasurements = testMeasurements()
    stationMean = fn -> :pollution_gen_server.getStationMean({20.06, 49.986}, "PM10") end |> :timer.tc |> elem(0)
    dailyMean = fn -> :pollution_gen_server.getDailyMean({2017, 5, 3}, "PM10") end |> :timer.tc |> elem(0)
    {"Stations: ", loadingStations, "Measurements: ", loadingMeasurements, "Station Mean: ", stationMean, "Daily Mean: ", dailyMean}
  end

  def testStations() do
    fn -> (PollutionDataStream.importLinesFromCSV |>
             Stream.map(fn(x) -> PollutionDataStream.convertLine(x) end) |>
             PollutionDataStream.identifyStations |>
             Stream.map(fn(x) -> PollutionDataStream.loadStation(x) end) |>
             Enum.reduce(fn(_, _) -> nil end)) end |>
      :timer.tc |> elem(0)
  end

  def testMeasurements() do
    fn -> (PollutionDataStream.importLinesFromCSV |>
             Stream.map(fn(x) -> PollutionDataStream.convertLine(x) end) |>
             Stream.map(fn(x) -> PollutionDataStream.loadMeasurement(x) end) |>
            Enum.reduce(fn(_, _) -> nil end)) end |>
      :timer.tc |> elem(0)
  end
end
