%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo <Erlang Programming>Example 5-1: About c/s model
%% the server functions 


-module(frequency).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, init/0]).



%% ====================================================================
%% start/0 functions
%% These are the start functions used to create and initialize the 
%% server.
%% ====================================================================
start() ->
    register(frequency, spawn(frequency, init, [])).



%% ====================================================================
%% init/0 functions
%% ====================================================================
init() ->
    Frequency = {get_frequencies(), []},
    loop(Frequency).



%% ====================================================================
%% get_frequencies/0 functions
%% ====================================================================
get_frequencies() ->
    [10, 11, 12, 13, 14, 15].



%% ====================================================================
%% loop/1 functions
%% ====================================================================
loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            {NewFrequencies, Reply} = deallocate(Frequencies, Freq),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, serverdetail} ->
            {Free, Allocated} = Frequencies,
            Detail = {{free, Free}, {allocated, Allocated}},
            reply(Pid, Detail),
            loop(Frequencies);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.



%% ====================================================================
%% reply/2 functions
%% ====================================================================
reply(Pid, Reply) ->
    Pid ! {reply, Reply}.



%% ====================================================================
%% allocate/2 functions
%% The Internal Help Function used to allocate and deallocate
%% frequencies.
%% ====================================================================
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq | Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}.



%% ====================================================================
%% deallocate/2 functions
%% ====================================================================
deallocate({Free, Allocated}, Freq) ->
    case lists:keymember(Freq, 1, Allocated) of
        false ->
            {{Free, Allocated}, {error, invalid_frequency}};
        _ ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {{[Freq | Free], NewAllocated}, {ok, Freq}}
    end.

