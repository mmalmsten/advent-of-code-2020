%%----------------------------------------------------------------------
%% Day 8: Handheld Halting
%%
%%----------------------------------------------------------------------
-module(puzzle08).

-export([start/0]).

start() ->
    Input = aoc:read_file("8.txt"),
    io:format("Puzzle 8, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 8, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%
%% Run your copy of the boot code. Immediately before any instruction is
%% executed a second time, what value is in the accumulator?
%%----------------------------------------------------------------------
part1(Input) ->
    {false, Acc} = game_console(Input, 0, 0, <<>>),
    Acc.

%%----------------------------------------------------------------------
%% Part 2
%%
%% Fix the program so that it terminates normally by changing exactly
%% one jmp (to nop) or nop (to jmp). What is the value of the
%% accumulator after the program terminates?
%%----------------------------------------------------------------------
part2(Input) ->
    part2(Input, <<>>, game_console(Input, 0, 0, <<>>)).

part2(<<Instruction:3/binary, Rest/binary>> = T, H,
      {false, _}) ->
    Input = case Instruction of
                <<"jmp">> -> <<H/binary, "nop", Rest/binary>>;
                <<"nop">> -> <<H/binary, "jmp", Rest/binary>>;
                _ -> <<"done">>
            end,
    [Current, Rest1] = case binary:split(T, <<"\n">>) of
                           [X1] -> [X1, <<>>];
                           X -> X
                       end,
    part2(Rest1,
          <<H/binary, Current/binary, "\n">>,
          game_console(Input, 0, 0, <<>>));
part2(_, _, Acc) -> Acc.

%%----------------------------------------------------------------------
%% Handheld game console
%%----------------------------------------------------------------------
game_console(<<>>, Acc, _, _) -> Acc;
game_console(<<"done", _/binary>>, Acc, _, _) ->
    {false, Acc};
%% acc increases or decreases a single global value called the
%% accumulator by the value given in the argument.
game_console(<<"acc ", Rest/binary>>, Acc, Self, H) ->
    [N, Rest1] = take_rest(Rest),
    game_console(Rest1,
                 Acc + aoc:binary_to_number(N),
                 Self + 1,
                 <<H/binary, "done\n">>);
%% jmp jumps to a new instruction relative to itself. The next
%% instruction to execute is found using the argument as an offset from
%% the jmp instruction.
game_console(<<"jmp ", Rest/binary>>, Acc, Self, H) ->
    [N, Rest1] = take_rest(Rest),
    Self1 = Self + aoc:binary_to_number(N),
    case aoc:binary_split_nth(<<H/binary, "done\n",
                                Rest1/binary>>,
                              Self1)
        of
        false -> game_console(<<"done">>, 0, 0, 0);
        {H1, T1} -> game_console(T1, Acc, Self1, H1)
    end;
%% nop stands for No OPeration - it does nothing. The instruction
%% immediately below it is executed next.
game_console(<<"nop ", Rest/binary>>, Acc, Self, H) ->
    [_, Rest1] = take_rest(Rest),
    game_console(Rest1,
                 Acc,
                 Self + 1,
                 <<H/binary, "done\n">>).

%%----------------------------------------------------------------------
%% Take remaining input in Binary. Return empty binary if no remaining
%% input exists.
%%----------------------------------------------------------------------
take_rest(Binary) ->
    case binary:split(Binary, <<"\n">>) of
        [X] -> [X, <<>>];
        X -> X
    end.

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    Input = aoc:read_file("8_mock.txt"),
    5 = part1(Input).

part2_test() ->
    Input = aoc:read_file("8_mock.txt"),
    8 = part2(Input).

-endif.
