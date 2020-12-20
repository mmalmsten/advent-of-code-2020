%%----------------------------------------------------------------------
%% Day 18: Operation Order
%%
%%----------------------------------------------------------------------
-module(puzzle18).

-export([same_precedence/1, start/0]).

start() ->
    Input = aoc:read_file("18.txt", <<"\n">>),
    % io:format("Puzzle 18, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 18, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
% not 46203334690
part1(Input) ->
    lists:sum([aoc:binary_to_number(same_precedence(remove_parentheses(I,
                                                                       same_precedence)))
               || I <- Input]).

%
%
%
remove_parentheses(Input, Eval) ->
    remove_parentheses(Input, Input, <<>>, Eval).

remove_parentheses(Input, <<>>, Input, _) -> Input;
remove_parentheses(Input, <<"(", Rest/binary>>, _,
                   Eval) ->
    remove_parentheses(Input, Rest, <<>>, Eval);
remove_parentheses(Input, <<")", _/binary>>, Before,
                   Eval) ->
    After = case Eval of
                same_precedence -> same_precedence(Before);
                advanced_math ->
                    list_to_binary(integer_to_list(advanced_math(Before)))
            end,
    Input1 = binary:replace(Input,
                            <<"(", Before/binary, ")">>,
                            After,
                            [global]),
    remove_parentheses(Input1, Input1, <<>>, Eval);
remove_parentheses(Input, <<N:1/binary, Rest/binary>>,
                   Before, Eval) ->
    remove_parentheses(Input,
                       Rest,
                       <<Before/binary, N/binary>>,
                       Eval).

%
%
%
same_precedence(Input) ->
    [Sum | I1] = binary:split(Input, <<" ">>, [global]),
    list_to_binary(integer_to_list(same_precedence(I1,
                                                   aoc:binary_to_number(Sum)))).

same_precedence([], Sum) -> Sum;
same_precedence([<<"*">>, Number | Rest], Sum) ->
    same_precedence(Rest,
                    Sum * aoc:binary_to_number(Number));
same_precedence([<<"+">>, Number | Rest], Sum) ->
    same_precedence(Rest,
                    Sum + aoc:binary_to_number(Number)).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) ->
    lists:sum([advanced_math(remove_parentheses(I,
                                                advanced_math))
               || I <- Input]).

advanced_math(Input) ->
    lists:foldl(fun (X, Sum) -> X * Sum end,
                1,
                [evaluate_expression(Expr)
                 || Expr <- binary:split(Input, <<"*">>, [global])]).

%%----------------------------------------------------------------------
%% Evaluate mathematical expression
%%----------------------------------------------------------------------
evaluate_expression(Expression) ->
    Expression1 =
        binary_to_list(binary:replace(<<Expression/binary,
                                        ".">>,
                                      <<" ">>,
                                      <<>>,
                                      [global])),
    {ok, Tokens, _} = erl_scan:string(Expression1),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    Result.

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    26 = part1(<<"2 * 3 + (4 * 5)">>),
    437 = part1(<<"5 + (8 * 3 + 9 + 3 * 4 * 3)">>),
    12240 = part1(<<"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 "
                    "* 4))">>),
    13632 = part1(<<"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) "
                    "+ 2 + 4 * 2">>).

part2_test() ->
    51 = part2(<<"1 + (2 * 3) + (4 * (5 + 6))">>),
    46 = part2(<<"2 * 3 + (4 * 5)">>),
    1445 = part2(<<"5 + (8 * 3 + 9 + 3 * 4 * 3)">>),
    669060 = part2(<<"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 "
                     "* 4))">>),
    23340 = part2(<<"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) "
                    "+ 2 + 4 * 2">>).

-endif.
