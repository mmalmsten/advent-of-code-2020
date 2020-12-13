%%----------------------------------------------------------------------
%% Day 13: Shuttle Search
%%
%%----------------------------------------------------------------------
-module(puzzle13).

-export([start/0]).

start() ->
    Input = aoc:read_file("13.txt", <<"\n">>),
    io:format("Puzzle 13, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 13, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1([Timestamp, Buses]) ->
    Buses1 = lists:map(fun (B) -> aoc:binary_to_number(B)
                       end,
                       [X
                        || X <- binary:split(Buses, <<",">>, [global]),
                           X /= <<"x">>]),
    {Bus, Timestamp1} = p1(aoc:binary_to_number(Timestamp),
                           Buses1),
    (Timestamp1 - aoc:binary_to_number(Timestamp)) * Bus.

p1(Timestamp, Buses) ->
    case [Bus || Bus <- Buses, Timestamp rem Bus == 0] of
        [] -> p1(Timestamp + 1, Buses);
        [B | _] -> {B, Timestamp}
    end.

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2([Timestamp, Buses]) ->
    Timestamp1 = aoc:binary_to_number(Timestamp),
    Buses1 = lists:map(fun (B) ->
                               case B of
                                   <<"x">> -> <<"x">>;
                                   _ -> aoc:binary_to_number(B)
                               end
                       end,
                       binary:split(Buses, <<",">>, [global])),
    p2(Timestamp1, Buses1, 1) - length(Buses1) + 1.

p2(Timestamp, [], _) -> Timestamp - 1;
p2(Timestamp, [<<"x">> | Buses], Step) ->
    p2(Timestamp + 1, Buses, Step);
p2(Timestamp, [Bus | Buses], Step)
    when Timestamp rem Bus == 0 ->
    p2(Timestamp + 1, Buses, aoc:lcm(Step, Bus));
p2(Timestamp, [Bus | Buses], Step) ->
    p2(Timestamp + Step, [Bus | Buses], Step).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    295 = part1([<<"939">>, <<"7,13,x,x,59,x,31,19">>]).

part2_test() ->
    3417 = part2([<<"0">>, <<"17,x,13,19">>]),
    754018 = part2([<<"0">>, <<"67,7,59,61">>]),
    779210 = part2([<<"0">>, <<"67,x,7,59,61">>]),
    1261476 = part2([<<"0">>, <<"67,7,x,59,61">>]),
    1202161486 = part2([<<"0">>, <<"1789,37,47,1889">>]).

-endif.
