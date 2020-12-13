%%----------------------------------------------------------------------
%% Day 12: Rain Risk
%%
%%----------------------------------------------------------------------
-module(puzzle12).

-export([start/0]).

start() ->
    Input = aoc:read_file("12.txt", <<"\n">>),
    io:format("Puzzle 12, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 12, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) ->
    {X, Y} = p1(Input, {0, 0}, 90),
    abs(X) + abs(Y).

p1([], Position, _) -> Position;
% Action N means to move north by the given value.
p1([<<"N", Val/binary>> | T], {WE, NS}, D) ->
    p1(T, {WE, NS - aoc:binary_to_number(Val)}, D);
% Action S means to move south by the given value.
p1([<<"S", Val/binary>> | T], {WE, NS}, D) ->
    p1(T, {WE, NS + aoc:binary_to_number(Val)}, D);
% Action E means to move east by the given value.
p1([<<"E", Val/binary>> | T], {WE, NS}, D) ->
    p1(T, {WE + aoc:binary_to_number(Val), NS}, D);
% Action W means to move west by the given value.
p1([<<"W", Val/binary>> | T], {WE, NS}, D) ->
    p1(T, {WE - aoc:binary_to_number(Val), NS}, D);
% Action L means to turn left the given number of degrees.
p1([<<"L", Val/binary>> | T], {WE, NS}, D) ->
    p1(T, {WE, NS}, D - aoc:binary_to_number(Val));
% Action R means to turn right the given number of degrees.
p1([<<"R", Val/binary>> | T], {WE, NS}, D) ->
    p1(T, {WE, NS}, D + aoc:binary_to_number(Val));
% Action F means to move forward by the given value in the D the ship is currently facing.
p1([<<"F", Val/binary>> | T], {WE, NS}, D) ->
    D1 = case D rem 360 of
             X when X < 0 -> 360 + X;
             X -> X
         end,
    Pos = case D1 of
              0 -> {WE, NS - aoc:binary_to_number(Val)};
              90 -> {WE + aoc:binary_to_number(Val), NS};
              180 -> {WE, NS + aoc:binary_to_number(Val)};
              270 -> {WE - aoc:binary_to_number(Val), NS}
          end,
    p1(T, Pos, D).

%%----------------------------------------------------------------------
%% Part 2
%% not 3348
%% not 7278
%% not 51418
%%----------------------------------------------------------------------
part2(Input) ->
    {X, Y} = p2(Input, {0, 0}, {10, 1}),
    abs(X) + abs(Y).

p2([], Pos, _) -> Pos;
p2([<<Char:1/binary, Val/binary>> | T], {WE, NS},
   {WpWE, WpNS}) ->
    Val1 = aoc:binary_to_number(Val),
    Wp = case Char of
             % Action N means to move the waypoint north by the given
             % value.
             <<"N">> -> {WpWE, WpNS + Val1};
             % Action S means to move the waypoint south by the given
             % value.
             <<"S">> -> {WpWE, WpNS - Val1};
             % Action E means to move the waypoint east by the given
             % value.
             <<"E">> -> {WpWE + Val1, WpNS};
             % Action W means to move the waypoint west by the given
             % value.
             <<"W">> -> {WpWE - Val1, WpNS};
             % Action L means to rotate the waypoint around the ship
             % left (counter-clockwise) the given number of degrees.
             <<"L">> ->
                 case Val1 of
                     0 -> {WpWE, WpNS};
                     270 -> {WpNS, -WpWE};
                     180 -> {-WpWE, -WpNS};
                     90 -> {-WpNS, WpWE}
                 end;
             % Action R means to rotate the waypoint around the ship
             % right (clockwise) the given number of degrees.
             <<"R">> ->
                 case Val1 of
                     0 -> {WpWE, WpNS};
                     90 -> {WpNS, -WpWE};
                     180 -> {-WpWE, -WpNS};
                     270 -> {-WpNS, WpWE}
                 end;
             _ -> {WpWE, WpNS}
         end,
    Pos = case Char of
              % Action F means to move forward to the waypoint a number
              % of times equal to the given value.
              <<"F">> -> {WE + WpWE * Val1, NS + WpNS * Val1};
              _ -> {WE, NS}
          end,
    io:format("~p~n", [{Pos, Wp}]),
    p2(T, Pos, Wp).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    25 = part1([<<"F10">>,
                <<"N3">>,
                <<"F7">>,
                <<"R90">>,
                <<"F11">>]).

part2_test() ->
    286 = part2([<<"F10">>,
                 <<"N3">>,
                 <<"F7">>,
                 <<"R90">>,
                 <<"F11">>]).

-endif.
