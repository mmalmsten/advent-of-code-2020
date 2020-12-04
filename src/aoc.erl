-module(aoc).

-export([all_keys_in_binary/2,
         binary_match_nth/3,
         binary_to_number/1,
         is_at_pos/3,
         is_numeric/1,
         read_at_pos/2,
         read_file/1,
         read_file/3,
         replace_at_pos/3,
         replace_list_at_pos/3,
         times_in_binary/3,
         value_between/3]).

value_between(Binary, Min, Max) ->
    case aoc:is_numeric(Binary) of
        true ->
            Binary1 = aoc:binary_to_number(Binary),
            Binary1 >= Min andalso Binary1 =< Max;
        _ -> false
    end.

binary_match_nth(Binary, Row_steps, Row_steps) ->
    Binary;
binary_match_nth(Binary, Row_steps, N) ->
    case binary:split(Binary, [<<"\n">>]) of
        [_, Binary1] ->
            binary_match_nth(Binary1, Row_steps, N + 1);
        _ -> false
    end.

all_keys_in_binary([], _) -> true;
all_keys_in_binary([Key | Keys], Passport) ->
    case binary:match(Passport, Key) of
        nomatch -> false;
        _ -> all_keys_in_binary(Keys, Passport)
    end.

is_numeric(N) ->
    Float = (catch binary_to_float(N)),
    Int = (catch binary_to_integer(N)),
    is_number(Float) orelse is_number(Int).

binary_to_number(N) ->
    case binary:match(N, <<".">>, []) of
        nomatch -> binary_to_integer(N);
        _ -> binary_to_float(N)
    end.

read_file(File) ->
    Priv_dir = code:priv_dir(advent_of_code_2020),
    {ok, Binary} = file:read_file([Priv_dir, "/", File]),
    Binary.

read_file(File, Delimiter, Types) ->
    Priv_dir = code:priv_dir(advent_of_code_2020),
    {ok, Binary} = file:read_file([Priv_dir, "/", File]),
    List = binary:split(Binary, Delimiter, [global]),
    case Types of
        numbers ->
            lists:map(fun (L) -> binary_to_number(L) end, List);
        _ -> List
    end.

% Check if a character exists at pos
is_at_pos(Binary, Pos, Character) ->
    Character == binary:part(Binary, {Pos, 1}).

% Read at pos
read_at_pos(Binary, Pos) ->
    binary:part(Binary, {Pos, 1}).

% Replace at pos in binary
replace_at_pos(<<_:1/binary, Rest/binary>>, 0,
               New_value) ->
    <<New_value/binary, Rest/binary>>;
replace_at_pos(Binary, Pos, New_value) ->
    <<Head:(Pos - 1)/binary, _:1/binary, Rest/binary>> =
        Binary,
    <<Head:(Pos - 1)/binary, New_value/binary,
      Rest/binary>>.

% Replace at pos in list
replace_list_at_pos(List, Pos, New_value) ->
    {Before, [_ | After]} = lists:split(Pos, List),
    lists:flatten([Before, [New_value], After]).

% Amount of times a Character occurs in a binary
times_in_binary(_, <<>>, N) -> N;
times_in_binary(Character,
                <<Character:1/binary, Tail/binary>>, N) ->
    times_in_binary(Character, Tail, N + 1);
times_in_binary(Character, <<_:1/binary, Tail/binary>>,
                N) ->
    times_in_binary(Character, Tail, N).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

binary_to_number_test() ->
    1 = binary_to_number(<<"1">>),
    1.1 = binary_to_number(<<"1.1">>).

read_file_test() ->
    <<"1,2,3,4,5,6,7,8,9">> = read_file("0.txt"),
    [1, 2, 3, 4, 5, 6, 7, 8, 9] = read_file("0.txt",
                                            <<",">>,
                                            numbers),
    [<<"1">>,
     <<"2">>,
     <<"3">>,
     <<"4">>,
     <<"5">>,
     <<"6">>,
     <<"7">>,
     <<"8">>,
     <<"9">>] =
        read_file("0.txt", <<",">>, any).

read_at_pos_test() ->
    <<"0">> = read_at_pos(<<"0123456789">>, 0),
    <<"2">> = read_at_pos(<<"0123456789">>, 2),
    <<"5">> = read_at_pos(<<"0123456789">>, 5).

% replace_at_pos_test() ->
%     <<"X123">> = replace_at_pos(<<"0123">>, 0, <<"X">>),
%     <<"01X3">> = replace_at_pos(<<"0123">>, 2, <<"X">>),
%     <<"012X">> = replace_at_pos(<<"0123">>, 3, <<"X">>).

-endif.
