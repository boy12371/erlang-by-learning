%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo factorial_system


-module(factorial_system).
-author("kuangyel2000@gmail.com").

-behaviour(application).
-export([start/2, start/0, stop/1, stop/0]).



%% 
%% ====================================================================
%% start/0, start/2
%% ====================================================================
start() ->
    application:start(?MODULE).

start(_Type, _Args) ->
    factorial_supervisor:start_link().



%% ====================================================================
%% stop/0, stop/1
%% ====================================================================
stop() ->
    application:stop(?MODULE).

stop(_State) ->
    ok.

