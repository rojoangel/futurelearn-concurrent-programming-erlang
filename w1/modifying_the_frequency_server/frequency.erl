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
%% 3> frequency:start().
%% true
%% 4> frequency ! {request, self(), allocate}.
%% {request,<0.64.0>,allocate}
%% 5> receive {reply,Reply} -> Reply end.
%% {ok,10}
%% 6> frequency ! {request, self(), {deallocate, 10}}.
%% {request,<0.64.0>,{deallocate,10}}
%% 7> f(Reply).
%% ok
%% 8> receive {reply,Reply} -> Reply end.
%% ok
%% 9> frequency ! {request, self(), stop}.
%% {request,<0.64.0>,stop}
%% 10> f(Reply).
%% ok
%% 11> receive {reply,Reply} -> Reply end.
%% stopped
%% 12>

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
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, {Freq,Pid}) ->
  case lists:member({Freq,Pid}, Allocated) of
    true -> 
      NewAllocated=lists:delete({Freq, Pid}, Allocated),
      {[Freq|Free],  NewAllocated , ok};
    false -> 
      {{Free, Allocated}, {error, cannot_deallocate_unused_frequency}}
  end.
