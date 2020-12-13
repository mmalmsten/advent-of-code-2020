-module(advent_of_code_2020).

-export([start/0]).

start() ->
    % puzzle01:start(),
    % puzzle02:start(),
    % puzzle03:start(),
    % puzzle04:start(),
    % puzzle05:start(),
    % puzzle06:start(),
    % puzzle07:start(),
    % puzzle08:start(),
    % puzzle09:start(),
    % puzzle10:start(),
    puzzle11:start(),
    % puzzle12:start(),
    % puzzle13:start(),
    loop().

loop() ->
    receive _ -> ok end,
    loop().
