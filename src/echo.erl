%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo <Erlang Programming>Exercise 4-1: An Echo Server


-module(echo).
-author("kuangyel2000@gmail.com").



%% ====================================================================
%% API functions
%% ====================================================================
-export([go/0, loop/0]).



%% ====================================================================
%% go/0 functions
%% ====================================================================
go() ->
    register(echo, spawn(echo, loop, [])),
    echo ! {self(), hello},
    receive
        {_Pid, Msg} ->
            io:format("~w~n", [Msg])
    end.



%% ====================================================================
%% loop/0 functions
%% ====================================================================
loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop ->
            true
    end.

