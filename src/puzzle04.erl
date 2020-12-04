%%----------------------------------------------------------------------
%% Day 4: Passport Processing
%%
%%----------------------------------------------------------------------
-module(puzzle04).

-export([start/0]).

start() ->
    Input = aoc:read_file("4.txt", <<"\n\n">>, binary),
    io:format("Puzzle 4, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 4, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) -> validate(part1, Input, 0).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) -> validate(part2, Input, 0).

%%----------------------------------------------------------------------
%% Number of passwords where all required fields are present (and valid
%% if part 2)
%%----------------------------------------------------------------------
validate(_, [], Valids) -> Valids;
validate(Part, [Passport | Passports], Valids) ->
    Valid = case aoc:all_keys_in_binary([<<"byr:">>,
                                         <<"iyr:">>,
                                         <<"eyr:">>,
                                         <<"hgt:">>,
                                         <<"hcl:">>,
                                         <<"ecl:">>,
                                         <<"pid:">>],
                                        Passport)
                     andalso
                     (Part == part1 orelse
                          lists:all(fun (Key) -> validate(Key) end,
                                    binary:split(Passport,
                                                 [<<" ">>, <<"\n">>],
                                                 [global])))
                of
                true -> 1;
                _ -> 0
            end,
    validate(Part, Passports, Valids + Valid).

% byr (Birth Year) - four digits; at least 1920 and at most 2002.
validate(<<"byr:", Year:4/binary>>) ->
    aoc:value_between(Year, 1920, 2002);
% iyr (Issue Year) - four digits; at least 2010 and at most 2020.
validate(<<"iyr:", Year:4/binary>>) ->
    aoc:value_between(Year, 2010, 2020);
% eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
validate(<<"eyr:", Year:4/binary>>) ->
    aoc:value_between(Year, 2020, 2030);
% hgt (Height) - a number followed by either cm or in:
validate(<<"hgt:", Height/binary>>) ->
    Height1 = binary:part(Height,
                          {0, byte_size(Height) - 2}),
    case binary:part(Height, {byte_size(Height), -2}) of
        % If cm, the number must be at least 150 and at most 193.
        <<"cm">> -> aoc:value_between(Height1, 150, 193);
        % If in, the number must be at least 59 and at most 76.
        <<"in">> -> aoc:value_between(Height1, 59, 76);
        _ -> false
    end;
% hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
validate(<<"hcl:#", Hex:6/binary>>) ->
    {match, [{0, 6}]} == re:run(Hex, "[0-9a-f]*");
% ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
validate(<<"ecl:amb">>) -> true;
validate(<<"ecl:blu">>) -> true;
validate(<<"ecl:brn">>) -> true;
validate(<<"ecl:gry">>) -> true;
validate(<<"ecl:grn">>) -> true;
validate(<<"ecl:hzl">>) -> true;
validate(<<"ecl:oth">>) -> true;
% pid (Passport ID) - a nine-digit number, including leading zeroes.
validate(<<"pid:", Id:9/binary>>) ->
    {match, [{0, 9}]} == re:run(Id, "[0-9]*");
% cid (Country ID) - ignored, missing or not.
validate(<<"cid:", _/binary>>) -> true;
% fallback
validate(_) -> false.

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    Input = aoc:read_file("4_mock_1.txt",
                          <<"\n\n">>,
                          binary),
    2 = part1(Input).

part2_test() ->
    Input = aoc:read_file("4_mock_2.txt",
                          <<"\n\n">>,
                          binary),
    4 = part2(Input).

-endif.
