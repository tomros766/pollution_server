Pollution server
=====

An OTP application developed as a continuous lab classes on Erlang and Elixir progeamming course.

The application provides server allowing to measure pollution levels from different measuring stations. 
###TODO

* Change comments from polish to english in polution module
* fix streams usage in pollution_data_stream module
* build hierarchy in src dir - group utils and training modules
* fix testing so the pass with simple `rebar3 eunit` command


Build
-----

    $ rebar3 compile
    $ rebar3 shell


Test
-----
The stacktrace print during tests is caused by an intentional server crash

    $ rebar3 test


## Modules description
### Pollution
The module contains structure of data - records `station` and `measurement`.
A single measurement station is represented as a map(**station** -> map(**measurement** -> value)).

The module contains also number of functions providing operations available on a monitor:
* adding a station
* adding a measurement value to a specific station
* removing a specific measurement
* getting a specific value depending on station and measurement parameters
* getting a mean of measurements of given type on a given station
* getting a daily mean from all stations
* getting a maximum value of given type and a station that has measured it
* getting a mean of measurements of given type on a given rectangular area

### PollutionServer
A client wrapper around previous module - creates more friendly API and guarantees basic app protection
**Not used anymore - replaced by PollutionGenServer**

### PollutionGenServer
From the client perspective basicly identical funcionality as PollutionServer, but it implements gen_server behaviour 
and because of that it delivers better application protection.

### PollutionData and PollutionDataStream
Purely training modules - they import data from pollution.csv file, perform operations on it and measure the time of execution
The Stream solution needs fixing.



