-module(client).
-export([start/0,loop/2]).
-export([init/0]).

% Initializes a client with random parameters
start() ->
    spawn(client, init, []).

init() ->
    process_flag(trap_exit,true),
    loop(rand:uniform(10) * 1000, rand:uniform(5) *1000).

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
  case allocate() of
    {ok, Freq} ->
      io:format("~w allocated ~w~n", [self(), Freq]),
      timer:sleep(TimeToKeepFreq),
      deallocate(Freq, TimeBetweenAllocations, TimeToKeepFreq),
      io:format("~w deallocated ~w~n", [self(), Freq]);
    {error, no_frequency} ->
      io:format("~w failed to allocate a frequency~n", [self()]);
    {error, server_down} -> % Handle fequency server down
      io:format("frequency server down. Sleeping...~n")
  end,
  timer:sleep(TimeBetweenAllocations),
  loop(TimeToKeepFreq, TimeBetweenAllocations).


% Override deallocate to deal with 'EXIT' message  
deallocate(Freq, TimeBetweenAllocations, TimeToKeepFreq) ->
    receive
        {'EXIT', _Pid, _Reason} -> % Checking for 'EXIT' msg before deallocating
            io:format("frequency server exited. Sleeping...~n"),
            timer:sleep(TimeBetweenAllocations), % Sleeping
            loop(TimeBetweenAllocations, TimeToKeepFreq) % loop as there's no need to deallocate
    after 0 -> % if there are no 'EXIT' msgs in the mailbox normal deallocate process
        frequency:deallocate(Freq)
    end.

% Override allocate to deal with server down
allocate() ->
    try frequency:allocate() of 
        Reply -> 
            Reply
    catch
        _Error:_Reason -> 
            {error, server_down}
    end.
                
