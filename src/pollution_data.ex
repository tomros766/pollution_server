defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV(name \\ "pollution.csv", minCount \\ 5900) do
    lines = File.read!(name) |> String.split("\n")
    if length(lines) < minCount do
      {:error, "Problem with importing, Wrong number of lines!"}
    end
    lines
  end

  def convertLine(line) do
    [date, time, longitude, latitude, value] = String.split(line, ",")
    parsedDate = date |> String.split("-") |> Enum.reverse() |> Enum.map( fn(x) -> elem(Integer.parse(x), 0) end) |> :erlang.list_to_tuple()
    parsedTime = time |> String.split(":") |> Enum.map( fn(x) -> elem(Integer.parse(x), 0) end) |> :erlang.list_to_tuple()
    {{x, _}, {y, _}} = {Float.parse(longitude), Float.parse(latitude)}
    location = {x, y}
    {numValue, _} = Float.parse(value)
    %{:datetime => {parsedDate, parsedTime}, :location => location, :pollutionLevel => numValue}
  end

  def identifyStations(mappedLines) do
    Enum.uniq_by(mappedLines, fn(m) -> m[:location] end)
  end

  def loadStations([]) do :ok end
  def loadStations([line | tail]) do
    :pollution_gen_server.addStation("station_#{elem(line[:location], 0)}_#{elem(line[:location], 1)}", line[:location])
    loadStations(tail)
  end

  def loadMeasurements([]) do :ok end
  def loadMeasurements([line | t]) do
    :pollution_gen_server.addValue(line[:location], line[:datetime], "PM10", line[:pollutionLevel])
    loadMeasurements(t)
  end

  def test do
    lines = importLinesFromCSV()
    converted = Enum.map(lines, fn(x) -> convertLine(x) end)
    stations = identifyStations(converted)
    loadingStations = fn -> loadStations(stations) end |> :timer.tc |> elem(0)
    loadingMeasurements = fn -> loadMeasurements(converted) end |> :timer.tc() |> elem(0)
    stationMean = fn -> :pollution_gen_server.getStationMean({20.06, 49.986}, "PM10") end |> :timer.tc |> elem(0)
    dailyMean = fn -> :pollution_gen_server.getDailyMean({2017, 5, 3}, "PM10") end |> :timer.tc |> elem(0)
    {"Stations: ", loadingStations, "Measurements: ", loadingMeasurements, "Station Mean: ", stationMean, "Daily Mean: ", dailyMean}
  end


end
