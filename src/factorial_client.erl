%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo factorial_client


-module(factorial_client).
-author("kuangyel2000@gmail.com").

%% ====================================================================
%% API functions
%% ====================================================================
-export([factorial/1, factorial/2]).



%% ====================================================================
%% Public functions
%% ====================================================================
%% start() ->
%%     factorial_server:start_link().
%% 
%% stop() ->
%%     factorial_server:stop().

factorial(Val) ->
    factorial_server:factorial(Val).

factorial(Val, IoDevice) ->
    factorial_server:factorial(Val, IoDevice).

