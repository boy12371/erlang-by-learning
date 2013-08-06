%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo factorial_logic


-module(factorial_logic).
-author("kuangyel2000@gmail.com").



%% ====================================================================
%% API functions
%% ====================================================================
-export([factorial/2, factorial/3, factorial_handler/0]).



%% ====================================================================
%% Public functions
%% factorial/2
%% ====================================================================
factorial(Int, Acc) when Int > 0 ->
    factorial(Int-1, Acc * Int);
factorial(0, Acc) ->
    Acc.



%% ====================================================================
%% factorial/3
%% ====================================================================
factorial(Int, Acc, IoDevice) when Int > 0 ->
    io:format(IoDevice, "Currect Factorial Log: ~p~n", [Acc]),
    factorial(Int - 1, Acc * Int, IoDevice);
factorial(0, Acc, IoDevice) ->
    io:format(IoDevice, "Factorial Results: ~p~n", [Acc]).



%% ====================================================================
%% factorial_handler/0
%% ====================================================================
factorial_handler() ->
    receive
        {factorial, Int} ->
            io:format("Factorial for ~p is ~p ~n", [Int, factorial(Int, 1)]),
            factorial_handler();
        {factorialRecorder, Int, File} ->
            {ok, IoDevice} = file:open(File, write),
            factorial(Int, 1, IoDevice),
            io:format("factorial recorder done. ~n", []),
            file:close(IoDevice),
            factorial_handler();
        Other ->
            io:format("Invaild Match for ~p~n", [Other]),
            factorial_handler()
    end.

