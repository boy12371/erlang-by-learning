%% @author Administrator
%% @doc @todo Add description to concurrent.


-module(concurrent).

%% ====================================================================
%% API functions
%% ====================================================================
-export([go/0, loop/0]).



%% ====================================================================
%% go/0 functions
%% ====================================================================
go() ->
    Pid = spawn(concurrent, loop, []),
    Pid ! {self(), hello},
    receive
        {Pid, Msg} ->
            io:format("~w~n", [Msg]),
            Pid ! stop
    end.

loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop ->
            true
    end.