%% @doc Simulates unexpected process failure. Erlang can be used to construct
%% very robust systems that can handle and recover
%% from unexpected errors. In order to test some of these features we need a way to
%% introduce random errors.

%% @author Karl Marklund <karl.marklund@it.uu.se>

-module(death).

-export([result/1, gamble/1]).


%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

seed() ->
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}).

result(0) ->
    live;
result(1) ->
    die;
result(P) ->
    seed(),
    R = rand:uniform(),
    if R >= P -> live;
       true -> die
    end.

%% @doc Gamle with death. The calling process will be terminated with probability
%% 0.0 &#x2264; P &#x2264; 1.0 and exit reason random_death.
-spec gamble(P) -> ok | no_return() when P::float().

gamble(P) when is_float(P), P >= 0, P =< 1.0 ->
    case result(P) of
        live -> ok;
        die -> exit(random_death)
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module.

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by death:test()

gamble_die_test() ->
    ?assertException(exit, random_death, gamble(1.0)).

gamble_live_test() ->
    R = [gamble(0.0) || _ <- lists:seq(1, 100)],
    E = [ok || _ <- lists:seq(1, 100)],
    ?assertMatch(E, R).


result_test_() ->
    Rates  = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6],

    Test = fun(P) ->
		   N = 1000,
		   R = [result(P) || _ <- lists:seq(1, N)],
		   {Dies, Lives} = lists:partition(fun(X) -> X == die end, R),
		   ND = length(Dies),
		   NL = length(Lives),
		   ?assertEqual(N, ND + NL),
		   P2 = ND/N,
		   io:format("~w, ~w~n", [P, P2]),
		   ?_assertEqual(true, P2 > (P - 0.01) orelse P < (P + 0.01)) end,
    [Test(Rate) || Rate <- Rates].
