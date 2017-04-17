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
% And to shutdown the entire system
% > client:stop().

loop(TimeBetweenAllocations, TimeToKeepFreq) ->
    timer:sleep(TimeBetweenAllocations),
    case allocate() of
        {ok, Freq} ->
            io:format("~w allocated ~w~n", [self(), Freq]),
            timer:sleep(TimeToKeepFreq),
            case deallocate(Freq) of 
                ok -> 
                    io:format("~w deallocated ~w~n", [self(), Freq]),
                    loop(TimeToKeepFreq, TimeBetweenAllocations);
                killed ->
                    io:format("~w Sleeping... frequency server killed~n", [self()]),
                    loop(TimeToKeepFreq, TimeBetweenAllocations);
                stopped ->
                    io:format("~w exiting - server stopped~n", [self()]),
                    exit(self(), normal)
            end;
        {error, no_frequency} ->
            io:format("~w Sleeping... failed to allocate a frequency~n", [self()]),
            loop(TimeBetweenAllocations, TimeToKeepFreq);
        {error, server_down} -> % Handle fequency server down
            io:format("~w Sleeping... frequency server down~n,", [self()]),
            loop(TimeBetweenAllocations, TimeToKeepFreq);
        stopped -> 
            io:format("~w exiting - server stopped~n", [self()]),
            exit(self(), normal);
        killed ->
            io:format("~w Sleeping... frequency server killed~n", [self()]),
            loop(TimeToKeepFreq, TimeBetweenAllocations)
    end.

% Override deallocate to deal with 'EXIT' message  
deallocate(Freq) ->
    receive
        {'EXIT', _Pid, killed} -> % Checking for 'EXIT' msg before deallocating
            killed;
        {'EXIT', _Pid, normal} ->
            stopped
    after 0 -> % if there are no 'EXIT' msgs in the mailbox normal deallocate process
        frequency:deallocate(Freq)
    end.

% Override allocate to deal with server down
allocate() ->
    receive
        {'EXIT', _Pid, killed} -> % Checking for 'EXIT' msg before deallocating
            killed;
        {'EXIT', _Pid, normal} ->
            stopped
    after 0 -> 
        try frequency:allocate() of 
            Reply -> 
                Reply
        catch
            _Error:_Reason -> 
                {error, server_down}
        end
    end.
