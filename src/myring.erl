%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo <Erlang Programming>Exercise 4-2: About benchmarking example


-module(myring).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start_proc/2]).



%% ====================================================================
%% start/1 functions
%% ====================================================================
start(Num) ->
    start_proc(Num, self()).



%% ====================================================================
%% start_proc/2 functions
%% ====================================================================
start_proc(0, Pid) ->
    Pid ! ok;
start_proc(Num, Pid) ->
    NPid = spawn(myring, start_proc, [Num - 1, Pid]),
    NPid ! ok,
    receive
        ok ->
            _ = factorial(1000),
            ok
    end.

factorial(0) ->
    1;
factorial(N) when is_integer(N), N > 0 ->
    N * factorial(N-1);
factorial(_N) ->
    {error, 'invalid integer'}.
