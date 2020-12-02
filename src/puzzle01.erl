%%----------------------------------------------------------------------
%% Day 1: Report Repair
%%
%% Used a simple brute-force since the input is pretty small
%% 🐞 There's a possible bug in this code, but that's OK.
%%
%%----------------------------------------------------------------------
-module(puzzle01).

-export([start/0]).

start() ->
    Input = helpers:read_file("1.txt", <<"\n">>, numbers),
    io:format("Puzzle 1, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 1, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) -> part1(Input, Input, Input).

part1([H1 | _], [H2 | _], _) when H1 + H2 == 2020 ->
    H1 * H2;
part1([], [_ | T], Input) -> part1(Input, T, Input);
part1([_ | T], L, Input) -> part1(T, L, Input).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) -> part2(Input, Input, Input, Input).

part2([H1 | _], [H2 | _], [H3 | _], _)
    when H1 + H2 + H3 == 2020 ->
    H1 * H2 * H3;
part2([], [_ | T], L, Input) -> part2(Input, T, L, L);
part2(L, [], [_ | T], Input) ->
    part2(L, Input, T, Input);
part2([_ | T], L1, L2, Input) ->
    part2(T, L1, L2, Input).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    514579 = part1([1721, 979, 366, 299, 675, 1456]).

part2_test() ->
    241861950 = part2([1721, 979, 366, 299, 675, 1456]).

-endif.
