- module(server).
- export([server/0,server/1]).

% Use it like this from the console
% - spawn the server
% ServerPid = spawn(server,server,[self()]).
% - send a message to the server
% ServerPid ! {check, "ABC"}.
% - check responses sent to the console
% flush().
server(ReplyPid) -> 
    receive 
        {check,Str} ->
            IsPalin = palin:palindrome(Str),
            ResultStr = case IsPalin of
                true -> " is a palindrome";
                false -> " is not a palindrome"
            end,
            ReplyPid ! {result, "\"" ++ Str ++ "\"" ++ ResultStr},
            server(ReplyPid);
        _ ->
            io:format("stopped~n")
    end.

% Modification to deal with different clients
% Use it like this from the console
% - spawn the server
% ServerPid = spawn(server,server,[]).
% - send a message to the server
% ServerPid ! {self(), check, "ABC"}.
% - check responses sent to the console
% flush().

server() ->
    receive 
        {ReplyPid,check,Str} ->
            IsPalin = palin:palindrome(Str),
            ResultStr = case IsPalin of
                true -> " is a palindrome";
                false -> " is not a palindrome"
            end,
            ReplyPid ! {result, "\"" ++ Str ++ "\"" ++ ResultStr},
            server();
        _ ->
            io:format("stopped~n")
    end.
    