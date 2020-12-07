%%----------------------------------------------------------------------
%% Day 7: Handy Haversacks
%%
%%----------------------------------------------------------------------
-module(puzzle07).

-export([start/0]).

-define(SEARCH_FOR, <<"shinygold">>).

start() ->
    Input = aoc:read_file("7.txt", <<".\n">>, binary),
    io:format("Puzzle 7, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 7, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) ->
    {Bags, Bags_map} = bag_content(Input),
    length(lists:filter(fun ({_, In_bag}) ->
                                search_in_bag(?SEARCH_FOR, In_bag, Bags_map, 0)
                                    > 0
                        end,
                        Bags)).

search_in_bag(_, [], _, N) -> N;
search_in_bag(Search, [{_, Search} | T], Map, N) ->
    search_in_bag(Search, T, Map, N + 1);
search_in_bag(Search, [{0, _} | T], Map, N) ->
    search_in_bag(Search, T, Map, N);
search_in_bag(Search, [{_, Bag} | T], Map, N) ->
    N1 = search_in_bag(Search,
                       maps:get(Bag, Map, []),
                       Map,
                       0),
    search_in_bag(Search, T, Map, N + N1).

%%----------------------------------------------------------------------
%% Part 2
%%----------------------------------------------------------------------
part2(Input) ->
    {Bags, Bags_map} = bag_content(Input),
    Gold_bag = maps:get(?SEARCH_FOR,
                        maps:from_list(Bags),
                        []),
    bags_in_bag(Gold_bag, Bags_map, 0).

bags_in_bag([], _, N) -> N;
bags_in_bag([{0, _} | T], Map, N) ->
    bags_in_bag(T, Map, N);
bags_in_bag([{I, Bag} | T], Map, N) ->
    N1 = bags_in_bag(maps:get(Bag, Map, []), Map, 0),
    bags_in_bag(T, Map, N + I + I * N1).

%%----------------------------------------------------------------------
%% Get bag content as list and map
%%----------------------------------------------------------------------
bag_content(Input) ->
    Bags = lists:flatten(lists:map(fun (Rule) -> b_c(Rule)
                                   end,
                                   Input)),
    {Bags, maps:from_list(Bags)}.

b_c(Rules) ->
    [Bag | Content] = binary:split(Rules,
                                   [<<" bags contain ">>, <<", ">>],
                                   [global]),
    Content1 = lists:map(fun (C) ->
                                 case binary:split(C, <<" ">>, [global]) of
                                     [<<"no">>, B1, B2 | _] ->
                                         {0, <<B1/binary, B2/binary>>};
                                     [Amount, B1, B2 | _] ->
                                         {aoc:binary_to_number(Amount),
                                          <<B1/binary, B2/binary>>}
                                 end
                         end,
                         Content),
    {binary:replace(Bag, <<" ">>, <<>>, [global]),
     Content1}.

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    Input = aoc:read_file("7_mock.txt", <<".\n">>, binary),
    4 = part1(Input).

part2_test() ->
    Input = aoc:read_file("7_mock.txt", <<".\n">>, binary),
    32 = part2(Input).

-endif.
