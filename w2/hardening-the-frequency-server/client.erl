-module(client).
-export([start/0,loop/2]).

% Initializes a client with random parameters
start() ->
    spawn(client,
          loop,
          [rand:uniform(10) * 1000, rand:uniform(5) *1000]).

% Client logic
% Allocates a frequency, sleeps and deallocates it
% Parameterized with 
% - time between allocations
% - time to keep frequency
%
% The whole system can be start by running
% > frequency:start().
% > client:start().
% > client:start().

loop(TimeBetweenAllocations, TimeToKeepFreq) ->
  case frequency:allocate() of
    {ok, Freq} ->
      io:format("~w allocated ~w~n", [self(), Freq]),
      timer:sleep(TimeToKeepFreq),
      frequency:deallocate(Freq),
      io:format("~w deallocated ~w~n", [self(), Freq]);
    {error, no_frequency} ->
      io:format("~w failed to allocate a frequency~n", [self()])
  end,
  timer:sleep(TimeBetweenAllocations),
  loop(TimeToKeepFreq, TimeBetweenAllocations).
    