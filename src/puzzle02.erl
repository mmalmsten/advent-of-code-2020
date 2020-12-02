%%----------------------------------------------------------------------
%% Day 2: Password Philosophy
%%
%%----------------------------------------------------------------------
-module(puzzle02).

-export([start/0]).

start() ->
    Input = helpers:read_file("2.txt", <<"\n">>, binary),
    io:format("Puzzle 2, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 2, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) ->
    length(lists:filter(fun (I) ->
                                [Min, Max, Letter, _, Password] =
                                    binary:split(I,
                                                 [<<"-">>, <<" ">>, <<":">>],
                                                 [global]),
                                Min1 = binary_to_integer(Min),
                                Max1 = binary_to_integer(Max),
                                Times = helpers:times_in_binary(Letter,
                                                                Password,
                                                                0),
                                Times >= Min1 andalso Times =< Max1
                        end,
                        Input)).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) ->
    length(lists:filter(fun (I) ->
                                [Pos1, Pos2, Letter, _, Password] =
                                    binary:split(I,
                                                 [<<"-">>, <<" ">>, <<":">>],
                                                 [global]),
                                helpers:is_at_pos(Password,
                                                  binary_to_integer(Pos1) - 1,
                                                  Letter)
                                    /=
                                    helpers:is_at_pos(Password,
                                                      binary_to_integer(Pos2) -
                                                          1,
                                                      Letter)
                        end,
                        Input)).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    2 = part1([<<"1-3 a: abcde">>,
               <<"1-3 b: cdefg">>,
               <<"2-9 c: ccccccccc">>]).

part2_test() ->
    1 = part2([<<"1-3 a: abcde">>,
               <<"1-3 b: cdefg">>,
               <<"2-9 c: ccccccccc">>]).

-endif.
