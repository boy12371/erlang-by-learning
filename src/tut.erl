% learning guard

-module(tut).
-export([start/1, start_proc/2, client/0]).

%% -define(URL, "http://127.0.0.1:8086/app/admin/admin").
%% -define(APPLICATION, "application/x-www-form-urlencoded").
%% -define(CALLING, "{\"methodName\":\"Login\",\"requestID\":2,\"token\":\"0897616560\",\"userName\":\"001\",\"password\":\"001\"}").
start(Num) ->
    start_proc(Num, self()).

start_proc(0, Pid) ->
    {closed, Pid};
start_proc(Num, Pid) ->
    spawn(fun() -> client() end),
    start_proc(Num - 1, Pid).



client() ->
%%     inets:start(),
%%     case httpc:request(post, {?URL, [], ?APPLICATION, lists:concat(["calling=" , ?CALLING])}, [], []) of
%%         {ok, Result} ->
%%             io:format("ok, retrun ~p~n",[Result]);
%%         {error, Reason} ->
%%             io:format("error cause ~p~n",[Reason])
%%     end.  
    {ok,Sock} = gen_tcp:connect("127.0.0.1", 8086, [{active, false}, {packet, 0}]),
    gen_tcp:send(Sock, "http://127.0.0.1:8086/app/admin/admin?calling={\"methodName\":\"Login\",\"requestID\":2,\"token\":\"0897616560\",\"userName\":\"001\",\"password\":\"001\"}"),
    A = gen_tcp:recv(Sock,0),
    gen_tcp:close(Sock),
    A.

