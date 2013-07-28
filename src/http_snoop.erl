%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo <Erlang Programming>Exercise 15-1: Snooping Http Request
%% Open a listener socket on your local machine.  


-module(http_snoop).
-author("kuangyel2000@gmail.com").



%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, listen/0]).



-define(PORT, 8086).
-define(TCP_OPTIONS, [{packet, 0}, {active, false}, {reuseaddr, true}]).



%% ====================================================================
%% start/1 functions
%% ====================================================================
start() ->
    process_flag(trap_exit, true),
    spawn(?MODULE, listen, []).



%% ====================================================================
%% stop/1 functions
%% ====================================================================
stop() ->
    exit(self(), kill),
    {ok, http_snoop_stop}.



%% ====================================================================
%% listen/1 functions
%% ====================================================================
listen() ->
    {ok, LSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    accept(LSocket).



%% ====================================================================
%% accept/1 functions
%% ====================================================================
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket) end),
    accept(LSocket).



%% ====================================================================
%% loop/1 functions
%% ====================================================================
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {ok, Logs} = file:open("http_snoop.log", [append, {encoding, utf8}]),
            io:format(Logs, "~ts", [list_to_binary(http_uri:decode(Data))]),
            io:format(Logs, "ip address: ~p date: ~p~n~n", [inet:peername(Socket), calendar:now_to_local_time(erlang:now())]),
            file:close(Logs),
            Url = amount(Data),
            io:format("~ts~n~n", [list_to_binary(http_uri:decode(Data))]),
            gen_tcp:send(Socket, Url),
            timer:sleep(1000),
            gen_tcp:close(Socket),
            loop(Socket);
        {error, closed} ->
            ok
    end.



%% ====================================================================
%% amount/1 functions
%% ====================================================================
amount(Data) ->
    case re:run(http_uri:decode(Data), ".+(\{.+:.+\}).*", [{capture, [1], list}]) of
        {match, [Url]} ->
            Url;
        _ ->
            Url = "{\"resultCode\":2}"
    end,
    list_to_binary(Url).

