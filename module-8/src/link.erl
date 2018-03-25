%% @doc Process link tutorial. When a link is setup
%% between two processes and one of the two processes terminates, the other
%% process automatically terminates with the same reason. Behind the scenes, the
%% Erlang runtime sends an exit signal to terminate the linked process. A
%% process that traps the exit signal will receive a special exit message
%% instead of terminating when a linked process terminates.

%% @author Karl Marklund <karl.marklund@it.uu.se>

-module(link).
-export([start/0]).

start() ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> worker() end),

    receive 
        {'EXIT', PID, Reason} ->
            io:format("Worker ~p terminated with reason ~w!~n", [PID, Reason])
    end.

worker() ->
    timer:sleep(3000),
    exit(some_error).
