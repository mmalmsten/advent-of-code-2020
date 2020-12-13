%%----------------------------------------------------------------------
%% Day 10: Adapter Array
%%
%%----------------------------------------------------------------------
-module(puzzle10).

-export([start/0]).

start() ->
    Input = aoc:read_file("10.txt", <<"\n">>, numbers),
    io:format("Puzzle 10, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 10, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) ->
    Input1 = lists:sort(Input),
    Built_in_adapter = lists:max(Input1) + 3,
    {X, _, Z} = p1(Input1 ++ [Built_in_adapter],
                   0,
                   {0, 0, 0}),
    X * Z.

p1([], _, Result) -> Result;
p1([H | T], B, {X, Y, Z}) when H - B == 1 ->
    p1(T, H, {X + 1, Y, Z});
p1([H | T], B, {X, Y, Z}) when H - B == 2 ->
    p1(T, H, {X, Y + 1, Z});
p1([H | T], B, {X, Y, Z}) when H - B == 3 ->
    p1(T, H, {X, Y, Z + 1}).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) ->
    Input1 = lists:sort(Input),
    Map1 = part2(Input1 ++ [lists:last(Input) + 3],
                 maps:put(0, 1, maps:new())),
    maps:get(lists:last(Input1), Map1).

part2([], Map) -> Map;
part2([H | T], Map) ->
    Map1 = maps:put(H,
                    maps:get(H - 3, Map, 0) + maps:get(H - 2, Map, 0) +
                        maps:get(H - 1, Map, 0),
                    Map),
    part2(T, Map1).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    35 = part1([16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]).

part2_test() ->
    8 = part2([16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]).

-endif.
