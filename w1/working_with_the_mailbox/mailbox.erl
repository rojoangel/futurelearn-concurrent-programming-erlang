- module(mailbox).
- export([receiver/0, delayedReceiver/0, receiver2/0, delayedReceiver2/0, receiver3/0, delayedReceiver3/0, delayedSequenceReceiver/0]).

% spawn it from the console like this:
% Pid = spawn(mailbox,receiver,[]).
% send messages to it like this:
% Pid ! hey.
% Pid ! ho.
% Pid ! letsgo.
%
receiver() -> 
    receive
        Msg -> 
            io:format("message:~w~n",[Msg]),
            receiver()
    end.

% spawn it from the console like this:
% Pid = spawn(mailbox,delayedReceiver,[]).
% send messages to it like this:
% Pid ! hey.
% Pid ! ho.
% Pid ! letsgo.
%
delayedReceiver() ->
    timer:sleep(10000),
    receiver().

% doesnt give priority to stop because matching is done 
% after receiving the message 
receiver2() -> 
    receive
        Msg -> 
            case Msg of
                stop -> 
                    io:format("message:~w~n",[Msg]);
                _ -> 
                    io:format("message:~w~n",[Msg]),
                    receiver2()
            end
    end.

% spawn it from the console like this:
% Pid = spawn(mailbox,delayedReceiver2,[]).
% send messages to it like this:
% Pid ! one.
% Pid ! two.
% Pid ! stop.
%
delayedReceiver2() ->
    timer:sleep(10000),
    receiver2().

% Behaves exactly as the one above
receiver3() -> 
    receive
        stop -> 
            io:format("message:~w~n",[stop]);
        Msg -> 
            io:format("message:~w~n",[Msg]),
            receiver3()
    end.

% spawn it from the console like this:
% Pid = spawn(mailbox,delayedReceiver3,[]).
% send messages to it like this:
% Pid ! one.
% Pid ! two.
% Pid ! stop.
%
delayedReceiver3() ->
    timer:sleep(10000),
    receiver3().

% Processess "first"" message and then "second" message
% spawn it from the console like this:
% Pid = spawn(mailbox,delayedSequenceReceiver,[]).
% send messages to it like this:
% Pid ! {first, one}.
% Pid ! {second, two}.
%
% or like this:
% Pid ! {second, two}.
% Pid ! {first, one}.
%
% result should be the same
delayedSequenceReceiver() ->
    timer:sleep(10000),
    receive 
        {first, FirstString} ->
            io:format("message:~w~n",[{first, FirstString}])
    end,
    receive
        {second, SecondString} ->
            io:format("message:~w~n",[SecondString]),
            delayedSequenceReceiver()
    end.
