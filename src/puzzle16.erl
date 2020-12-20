%%----------------------------------------------------------------------
%% Day 16: Ticket Translation
%%
%%----------------------------------------------------------------------
-module(puzzle16).

-export([start/0]).

start() ->
    Input = aoc:read_file("16.txt", <<"\n\n">>),
    io:format("Puzzle 16, part 1: ~p~n", [part1(Input)]),
    io:format("Puzzle 16, part 2: ~p~n", [part2(Input)]).

%%----------------------------------------------------------------------
%% Part 1
%%----------------------------------------------------------------------
part1(Input) ->
    {Ticket_fields, _My_ticket, Nearby_tickets} =
        read_ticket_data(Input),
    part1(lists:sort(lists:flatten(maps:values(Ticket_fields))),
          lists:flatten(Nearby_tickets)).

part1(Ticket_fields, Nearby_tickets) ->
    lists:sum([N
               || N <- Nearby_tickets,
                  lists:member(N, Ticket_fields) == false]).

%%----------------------------------------------------------------------
%% Part 2
%% Apologies for the ugly code...
%%----------------------------------------------------------------------
part2(Input) ->
    {Ticket_fields, My_ticket, Nearby_tickets} =
        read_ticket_data(Input),
    Ticket_fields1 =
        lists:flatten(maps:values(Ticket_fields)),
    Nearby_tickets1 = lists:filter(fun (T) ->
                                           lists:all(fun (T1) ->
                                                             lists:member(T1,
                                                                          Ticket_fields1)
                                                     end,
                                                     T)
                                   end,
                                   Nearby_tickets),
    Nearby_ticket_keys = [[lists:nth(N, Nt)
                           || Nt <- Nearby_tickets1]
                          || N <- lists:seq(1, length(My_ticket))],
    Result = p2(1, Nearby_ticket_keys, Ticket_fields, []),
    Result1 = lists:foldl(fun (X, Map) -> maps:merge(Map, X)
                          end,
                          #{},
                          [M || {_, M, _} <- Result]),
    p21(maps:keys(Ticket_fields), My_ticket, Result1, []).

p2(_, [], _, Result) ->
    lists:reverse(lists:sort(Result));
p2(Nth, [H | T], Ticket_fields, Result) ->
    Map = maps:filter(fun (_, M) -> M == Nth end,
                      maps:map(fun (_, V) ->
                                       case lists:all(fun (V1) -> V1 end,
                                                      [lists:member(H1, V)
                                                       || H1 <- H])
                                           of
                                           true -> Nth;
                                           _ -> null
                                       end
                               end,
                               Ticket_fields)),
    p2(Nth + 1,
       T,
       Ticket_fields,
       [{maps:size(Map), Map, Nth} | Result]).

p21([], _, _, Result) ->
    lists:foldl(fun (R, Sum) -> R * Sum end, 1, Result);
p21([<<"departure", _/binary>> = H | T], My_ticket, Pos,
    Result) ->
    #{H := Nth} = Pos,
    p21(T,
        My_ticket,
        Pos,
        [lists:nth(Nth, My_ticket) | Result]);
p21([_ | T], My_ticket, Pos, Result) ->
    p21(T, My_ticket, Pos, Result).

%%----------------------------------------------------------------------
%%
%%----------------------------------------------------------------------
read_ticket_data(Input) ->
    [Ticket_fields,
     <<"your ticket:\n", My_ticket/binary>>,
     <<"nearby tickets:\n", Nearby_tickets/binary>>] =
        Input,
    Ticket_fields1 =
        ticket_fields(binary:split(Ticket_fields,
                                   <<"\n">>,
                                   [global]),
                      #{}),
    My_ticket1 = split_ticket(My_ticket),
    Nearby_tickets1 = [split_ticket(T)
                       || T
                              <- binary:split(Nearby_tickets,
                                              <<"\n">>,
                                              [global])],
    {Ticket_fields1, My_ticket1, Nearby_tickets1}.

split_ticket(Ticket) ->
    [list_to_integer(binary_to_list(T))
     || T <- binary:split(Ticket, <<",">>, [global])].

ticket_fields([], Map) -> Map;
ticket_fields([Ticket_field | Ticket_fields], Map) ->
    [Key, Value] = binary:split(Ticket_field, <<": ">>),
    Ranges = [binary:split(V, <<"-">>)
              || V <- binary:split(Value, <<" or ">>, [global])],
    Values =
        lists:flatten([lists:seq(list_to_integer(binary_to_list(R1)),
                                 list_to_integer(binary_to_list(R2)))
                       || [R1, R2] <- Ranges]),
    ticket_fields(Ticket_fields,
                  maps:put(Key, Values, Map)).

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    Input = aoc:read_file("16_mock_1.txt", <<"\n\n">>),
    71 = part1(Input).

part2_test() ->
    Input = aoc:read_file("16_mock_2.txt", <<"\n\n">>),
    855438643439 = part2(Input).

-endif.
