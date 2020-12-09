%%----------------------------------------------------------------------
%% Day 9: Encoding Error
%%
%%----------------------------------------------------------------------
-module(puzzle09).

-export([start/0]).

-ifdef(TEST).

-define(NUMBER_OF_NUMBERS, 5).

-else.

-define(NUMBER_OF_NUMBERS, 25).

-endif.

start() ->
    Input = aoc:read_file("9.txt", <<"\n">>, numbers),
    io:format("Puzzle 9, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 9, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) ->
    {Before, List} = lists:split(?NUMBER_OF_NUMBERS, Input),
    part1(List, lists:reverse(Before)).

part1([], _) -> false;
part1([H | T], Before) ->
    case numbers_sum(H, Before, 1, 2) of
        true -> part1(T, [H | lists:droplast(Before)]);
        false -> H
    end.

numbers_sum(_, _, (?NUMBER_OF_NUMBERS) + 1, _) -> false;
numbers_sum(H, List, N, (?NUMBER_OF_NUMBERS) + 1) ->
    numbers_sum(H, List, N + 1, 1);
numbers_sum(H, List, N, N) ->
    numbers_sum(H, List, N, N + 1);
numbers_sum(H, List, N1, N2) ->
    case lists:nth(N1, List) + lists:nth(N2, List) of
        H -> true;
        _ -> numbers_sum(H, List, N1, N2 + 1)
    end.

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) ->
    Invalid_number = part1(Input),
    contiguous_set(Invalid_number, Input, 0).

contiguous_set(Invalid_number, [_ | T] = List, N) ->
    Sublist = lists:sublist(List, N),
    case lists:sum(Sublist) of
        Invalid_number ->
            lists:min(Sublist) + lists:max(Sublist);
        X when X > Invalid_number ->
            contiguous_set(Invalid_number, T, 0);
        _ -> contiguous_set(Invalid_number, List, N + 1)
    end.

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    Input = aoc:read_file("9_mock.txt", <<"\n">>, numbers),
    127 = part1(Input).

part2_test() ->
    Input = aoc:read_file("9_mock.txt", <<"\n">>, numbers),
    62 = part2(Input).

-endif.
