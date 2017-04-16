# Hardening the frequency server - trying it for yourself

**We have introduced process linking, exit signals, and “trapping exits”, as well as seeing how these can be applied to the frequency server example. In this exercise we’ll look in more detail at the example, and use the ‘observer’ tool to help us see what is going in when parts of a system fail.**

Use the comments here to share your approaches and solutions to this exercise. In the next step, we’ll have a discussion about how you’ve got on.

There is a supporting file, frequency_hardened.erl, available (as a zip file) under ‘Downloads’ below. This is an update of the frequency.erl files from previous steps - the file itself is called frequency_hardened.erl to distinguish it from the original, but it still defines the frequency module so you may want to re-name the file.

## Modelling clients
In practice, a server will have multiple clients, and in order to see how the system behaves, implement a client function that, when spawned as a process, can be used to model a client. To allow a simulation to go on indefinitely, your function should cycle through a number of allocations and deallocations, forever (since the server is designed to live indefinitely, too).

We would like to model systems where there are multiple clients, so it would be useful to parameterise the client so that it can behave in different ways when called with different parameters.

## The observer tool
Erlang comes with a GUI-based tool called the observer, which allows us to see, among other things, all the processes running in a system at any time, including their particular Pid.

To run the observer, type `observer:start()` in the erlang shell. You will see this tabbed window, which opens with an overview of the system:

![observer tabbed window](https://ugc.futurelearn.com/uploads/assets/51/0d/large_hero_510d69a0-1bf2-49bd-ba30-e8972970c12e.jpg)

Selecting the ***Processes*** tab will give a view like this:

![observer processes tab](https://ugc.futurelearn.com/uploads/assets/ec/e9/large_hero_ece90f09-dd71-4f4e-874c-84bbaf3741bd.jpg)

in which you can see the processes listed.

## Observing the frequency server and clients.
Using the observer, set up a system of frequency server and at least two clients, and kill the frequency server when the clients are in possession of a frequency – you can make the clients “sleep” at any point using the timer:sleep/1 function – observe that the clients are killed too.

How would you modify the clients so that they are not affected by the server terminating? How could you then shut down the entire system if you needed to?