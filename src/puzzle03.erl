%%----------------------------------------------------------------------
%% Day 3: Toboggan Trajectory
%%
%%----------------------------------------------------------------------
-module(puzzle03).

-export([start/0]).

start() ->
    Input = helpers:read_file("3.txt"),
    io:format("Puzzle 3, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 3, part 2: ~p~n", [part2(Input)]).

-ifdef(TEST).

-define(COLUMNS, 10).

-else.

-define(COLUMNS, 30).

-endif.

-define(SLOPES,
        [{1, 1}, {1, 3}, {1, 5}, {1, 7}, {2, 1}]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) -> walk(Input, 1, {0, 3}, 0).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) ->
    lists:foldl(fun ({Step_row, Step_column}, Sum) ->
                        Sum * walk(Input, Step_row, {0, Step_column}, 0)
                end,
                1,
                ?SLOPES).

%%----------------------------------------------------------------------
%% Walk the slope
%%----------------------------------------------------------------------
walk(<<>>, _, _, Trees) -> Trees;
walk(Input, Step_row, {Column, Step_column}, Trees)
    when Column > (?COLUMNS) ->
    walk(Input,
         Step_row,
         {Column - ((?COLUMNS) + 1), Step_column},
         Trees);
walk(Input, Step_row, {Column, Step_column}, Trees) ->
    Tree = case binary:part(Input, {Column, 1}) of
               <<"#">> -> 1;
               _ -> 0
           end,
    case nth_match(Input, Step_row, 0) of
        false -> Trees + Tree;
        Input1 ->
            walk(Input1,
                 Step_row,
                 {Column + Step_column, Step_column},
                 Trees + Tree)
    end.

nth_match(Binary, Row_steps, Row_steps) -> Binary;
nth_match(Binary, Row_steps, N) ->
    case binary:split(Binary, [<<"\n">>]) of
        [_, Binary1] -> nth_match(Binary1, Row_steps, N + 1);
        _ -> false
    end.

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    Input = helpers:read_file("3_mock.txt"),
    7 = part1(Input).

part2_test() ->
    Input = helpers:read_file("3_mock.txt"),
    336 = part2(Input).

-endif.
