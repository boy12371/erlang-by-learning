-module(tiny).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
   start_process().

stop(_State) ->
   ok.

start_process() ->
   FLoop =
   fun() ->
       Fun =
       fun(F) ->
           receive Msg ->
               io:format("receive msg:~p~n", [Msg]),
               F(F)
           after 1000 ->
               {ok, App} = application:get_application(self()),
               io:format(user, "~p 1.0 running...~n", [App]),
               F(F)
           end
       end,
       Fun(Fun)
   end,
   {ok, proc_lib:spawn_link(FLoop)}.
