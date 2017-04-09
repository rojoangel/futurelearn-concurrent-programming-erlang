%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0,start/0]).

%% Sample use of frequency process
%% 1> c(frequency).
%% {ok,frequency}
%% 2> frequency:start().
%% true
%% 3> frequency ! {request, self(), allocate}.
%% {request,<0.57.0>,allocate}
%% 4> receive {reply, Msg} -> Msg end.
%% {ok,10}
%% 5> frequency ! {request, self(), allocate}.
%% {request,<0.57.0>,allocate}
%% 6> f(Msg).
%% ok
%% 7>  receive {reply, Msg} -> Msg end.
%% {error,cannot_allocate_multiple_frequencies}
%% 8> frequency ! {request, self(), {deallocate, 11}}.
%% {request,<0.57.0>,{deallocate,11}}
%% 9> f(Msg).
%% ok
%% 10> receive {reply, Msg} -> Msg end.
%% {error,cannot_deallocate_unused_frequency}
%% 11> frequency ! {request, self(), {deallocate, 10}}.
%% {request,<0.57.0>,{deallocate,10}}
%% 12> f(Msg).
%% ok
%% 13> receive {reply, Msg} -> Msg end.
%% ok
%% 14> frequency ! {request, self(), stop}.
%% {request,<0.57.0>,stop}
%% 15> f(Msg).
%% ok
%% 16> receive {reply, Msg} -> Msg end.
%% stopped
%% 17>

%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(frequency, spawn(frequency,init,[])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, {Freq,Pid}),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case lists:keysearch(Pid, 2, Allocated) of 
    false ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
    _ ->  
      {{Free, Allocated}, {error, cannot_allocate_multiple_frequencies}}
  end.

deallocate({Free, Allocated}, {Freq,Pid}) ->
  case lists:member({Freq,Pid}, Allocated) of
    true -> 
      NewAllocated=lists:delete({Freq, Pid}, Allocated),
      {{[Freq|Free],  NewAllocated} , ok};
    false -> 
      {{Free, Allocated}, {error, cannot_deallocate_unused_frequency}}
  end.
