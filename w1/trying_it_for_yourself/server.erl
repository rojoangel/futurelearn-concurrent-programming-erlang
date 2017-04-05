- module(server).
- export([server/0,server/1,client/1,frontend/2]).

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
            io:format("Processed by ~p~n", [self()]), % instrumentation
            server();
        _ ->
            io:format("stopped~n")
    end.

% Server client
% Use it like this form the console
% - spawn the server
% ServerPid = spawn(server,server,[]).
% - spawn the client
% spawn(server,client,[ServerPid]).

client(ServerPid) ->
    ServerPid ! {self(), check, "Dabale arroz a la zorra el abad"},
    receive
        {result, Msg} -> 
            io:format(Msg++"~n")
    end.

% Server front end
% Use it like this form the console
% - spawn the servers
% Server1Pid = spawn(server,server,[]).
% Server2Pid = spawn(server,server,[]).
% - spawn the client
% FrontEndPid = spawn(server,frontend,[Server1Pid,Server2Pid]).
% - send a message to the frontend
% FrontEndPid ! {self(), check, "ABC"}.
% - get responses flush().

frontend(Server1Pid, Server2Pid) ->
    receive
        Msg ->
            Server1Pid ! Msg,
            frontend(Server2Pid, Server1Pid) % reverting the order to alternate servers
    end.