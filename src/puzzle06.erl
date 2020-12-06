%%----------------------------------------------------------------------
%% Day 6: Custom Customs
%%
%%----------------------------------------------------------------------
-module(puzzle06).

-export([start/0]).

start() ->
    Input = aoc:read_file("6.txt", <<"\n\n">>, binary),
    io:format("Puzzle 6, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 6, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) ->
    lists:foldl(fun (Answers, Sum) ->
                        Sum + maps:size(binary_occurences(Answers))
                end,
                0,
                Input).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) ->
    lists:foldl(fun (Answers, Sum) ->
                        P = length(binary:split(Answers, <<"\n">>, [global])),
                        Sum +
                            maps:size(maps:filter(fun (_, V) -> P == V end,
                                                  binary_occurences(Answers)))
                end,
                0,
                Input).

%%----------------------------------------------------------------------
%% Get a map with each character as key and the number of occurences
%% for each key as values. Ignore newlines
%%----------------------------------------------------------------------
binary_occurences(Binary) ->
    binary_occurences(binary:replace(Binary,
                                     <<"\n">>,
                                     <<>>,
                                     [global]),
                      #{}).

binary_occurences(<<>>, Occurences) -> Occurences;
binary_occurences(<<Char:1/binary, Rest/binary>>,
                  Occurences) ->
    binary_occurences(Rest,
                      maps:put(Char,
                               maps:get(Char, Occurences, 0) + 1,
                               Occurences)).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    Input = aoc:read_file("6_mock.txt", <<"\n\n">>, binary),
    11 = part1(Input).

part2_test() ->
    Input = aoc:read_file("6_mock.txt", <<"\n\n">>, binary),
    6 = part2(Input).

-endif.
