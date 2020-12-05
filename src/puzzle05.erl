%%----------------------------------------------------------------------
%% Day 5: Binary Boarding
%%
%%----------------------------------------------------------------------
-module(puzzle05).

-export([start/0]).

-define(ROWS, 127).

-define(COLUMNS, 7).

-define(ID_PARAM, 8).

start() ->
    Input = aoc:read_file("5.txt", <<"\n">>, binary),
    io:format("Puzzle 5, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 5, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) ->
    round(lists:max(lists:map(fun (I) ->
                                      seat_id(I, 0, ?ROWS)
                              end,
                              Input))).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) ->
    round(find_seat(lists:sort(lists:map(fun (I) ->
                                                 seat_id(I, 0, ?ROWS)
                                         end,
                                         Input)))).

find_seat([H1, H2 | _]) when 2 == H2 - H1 -> H1 + 1;
find_seat([_ | T]) -> find_seat(T).

%%----------------------------------------------------------------------
%% Get seat ID
%%----------------------------------------------------------------------
seat_id(<<>>, _, Column) -> Column;
%% Only 3 characters left in binary, use those for getting the column
seat_id(<<C:3/binary>>, Row, _) when Row > 0 ->
    Row * (?ID_PARAM) + seat_id(C, 0, ?COLUMNS);
%% R == B
seat_id(<<"R", Rest/binary>>, Min, Max) ->
    seat_id(<<"B", Rest/binary>>, Min, Max);
%% L == F
seat_id(<<"L", Rest/binary>>, Min, Max) ->
    seat_id(<<"F", Rest/binary>>, Min, Max);
%% F means to take the lower half
seat_id(<<"F", Rest/binary>>, Min, Max) ->
    seat_id(Rest, Min, math:floor((Max - Min) / 2 + Min));
%% B means to take the upper half
seat_id(<<"B", Rest/binary>>, Min, Max) ->
    seat_id(Rest, math:ceil((Max - Min) / 2 + Min), Max).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    357 = part1([<<"FBFBBFFRLR">>]),
    567 = part1([<<"BFFFBBFRRR">>]),
    119 = part1([<<"FFFBBBFRRR">>]),
    820 = part1([<<"BBFFBBFRLL">>]).

part2_test() -> ok.

-endif.
