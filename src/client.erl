%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo <Erlang Programming>Example 5-1: About c/s model
%% the client functions 


-module(client).
-author("kuangyel2000@gmail.com").



%% ====================================================================
%% API functions
%% ====================================================================
-export([stop/0, serverdetail/0, allocate/0, deallocate/1]).



%% ====================================================================
%% stop/0 functions
%% ====================================================================
stop() ->
    call(stop).



%% ====================================================================
%% serverdetail/0 functions
%% ====================================================================
serverdetail() ->
    call(serverdetail).



%% ====================================================================
%% allocate/0 functions
%% ====================================================================
allocate() ->
    call(allocate).



%% ====================================================================
%% deallocate/1 functions
%% ====================================================================
deallocate(Freq) ->
    call({deallocate, Freq}).



%% ====================================================================
%% call/1 functions
%% We hide all message passing and the message protocol in a functional
%% interface.
%% ====================================================================
call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} ->
            Reply
    end.

