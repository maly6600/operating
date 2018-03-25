%% @doc A state full FIFO queue process. A common pattern in Erlang is to
%% provide services as separate processes. If we keep the FIFO queue as a
%% separate process, we can use the PID to the FIFO process as a reference to
%% the FIFO. When we update the state of the FIFO, the PID remains the same.

%% @author Karl Marklund <karl.marklund@it.uu.se>

-module(sfifo).

-export([new/0, size/1, push/2, pop/1, empty/1]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

%% A common pattern is Erlang is to provide services as separate
%% processes. In this module we spawn a new process to keep the state
%% of a FIFO queue.


%% @doc Creates a new FIFO queue.
-opaque sfifo()::pid().
-spec new() -> sfifo().

new() ->
    spawn(fun() -> loop(fifo:new()) end).

%% This is the stateful process loop.

loop(Fifo) ->
    receive
	{size, PID} ->
	    PID ! {size, fifo:size(Fifo)},
	    loop(Fifo);
	{empty, PID} ->
	    PID ! fifo:empty(Fifo),
	    loop(Fifo);
        {pop, PID} ->
            case fifo:empty(Fifo) of
                true ->
                    PID ! {error, 'empty fifo'},
                    loop(Fifo);
                false ->
                    {Value, Fifo2} = fifo:pop(Fifo),
                    PID ! Value,
                    loop(Fifo2)
            end;
        {push, PID, Value} ->             
            Fifo2 = fifo:push(Fifo, Value),
            PID ! ok,
            loop(Fifo2)
    end.


%%  By hiding the message passing protocol inside a functional
%%  interface the user of the FIFO doesn't need to know whether or not
%%  the FIFO is implemented as a separate process.

%% @doc Returns the number of elements in Fifo.
-spec size(Fifo) -> integer() when Fifo::sfifo().

size(Fifo) ->
    Fifo ! {size, self()},
    receive
	{size, Size} ->
	    Size
    end.

%% @doc Returns true if Fifo is empty, otherwise returns false.
-spec empty(Fifo) -> true|false when Fifo::sfifo().

empty(Fifo) ->
    Fifo ! {empty, self()},
    receive
	true ->
	    true;
	false  ->
	    false
    end.

%% @doc Pops a value from Fifo.
-spec pop(Fifo) -> term() when Fifo::sfifo().

pop(Fifo) ->
    Fifo ! {pop, self()},
    receive
        Value ->
            Value
    end.

    


%% @doc Push a new value to Fifo.
-spec push(Fifo, Value) -> ok when
      Fifo::sfifo(),
      Value::term().
push(Fifo, Value) ->
    Fifo ! {push, self(), Value},
    receive
        ok ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the sfifo:test() function to this module.

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by sfifo:test()

start_test_() ->
    [?_assertMatch(true,                      is_pid(new())),
     ?_assertMatch(0,                     sfifo:size(new())),
     ?_assertMatch(true,                       empty(new())),
     ?_assertMatch({error, 'empty fifo'},        pop(new()))].

empty_test() ->
    F =  new(),
    ?assertMatch(true,  empty(F)),
    push(F, foo),
    ?assertMatch(false, empty(F)),
    pop(F),
    ?assertMatch(true,  empty(F)).

push_pop_test() ->
    F = new(),
    push(F, foo),
    push(F, bar),
    push(F, luz),
    ?assertMatch(false,               empty(F)),
    ?assertMatch(foo,                   pop(F)),
    ?assertMatch(bar,                   pop(F)),
    ?assertMatch(luz,                   pop(F)),
    ?assertMatch({error, 'empty fifo'}, pop(F)).

large_push_pop_test() ->
    F = new(),
    List = lists:seq(1, 999),
    [push(F, Value) || Value <- List],
    ?assertEqual([pop(F) || _ <- List], List).
