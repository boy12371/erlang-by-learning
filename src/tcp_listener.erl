%% @author Richard
%% @doc @todo One of the shortcomings of the gen_tcp module is that it
%% only exports interface to a blocking accept call.Examining
%% prim_inet module reveals an interesting fact that the actual call
%% to inet driver to accept a client socket is asynchronous. While
%% this is a non-documented property, which means that the OTP team is
%% free to change this implementation, we will exploit this
%% functionality in the construction of our server. 


-module(tcp_listener).
-author("kuangyel2000@gmail.com").

-behaviour(gen_server).



%% ====================================================================
%% External API
%% ====================================================================
-export([start_link/2]).



%% ====================================================================
%% gen_server callbacks
%% ====================================================================
-export([   init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            terminate/2,
            code_change/3
        ]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state,
            {    listener, % Listening socket
                 acceptor, % Asynchronous acceptor's internal reference
                 module    % FSM handling module
            }
       ).



%% ====================================================================
%% Func: start_link/2
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the listening process.
%% @end
%% ====================================================================
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link(  {local, ?MODULE},
                            ?MODULE,
                            [Port, Module],
                            []
                         ).



%% ====================================================================
%% Callback functions from gen_server
%% Func: init/1
%% In this module init/1 call takes two parameters - the port number
%% that the TCP listener should be started on and the name of a
%% protocol handling module for client connections.The initialization
%% function opens a listening socket in passive {active, false} mode.
%% This is done so that we have flow control of the data received on
%% the connected client sockets that will inherit this option from the
%% listening socket.The most interesting part of this code is the
%% prim_inet:async_accept/2 call as well as the handling of
%% asynchronous inet_async messages.
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%% ====================================================================
init([Port, Module]) ->
    process_flag(trap_exit, true),
    Opts = [    binary,
                {packet, 2},
                {reuseaddr, true},
                {keepalive, true},
                {backlog, 30},
                {active, false}
           ],
    case gen_tcp:listen(Port, Opts) of
        {ok, Listen_socket} ->
            %% Create first accepting process
            {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
            {ok, #state
                {   listener = Listen_socket,
                    acceptor = Ref,
                    module   = Module
                }
            };
        {error, Reason} ->
            {stop, Reason}
    end.



%% ====================================================================
%% Func: handle_call/3
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%% ====================================================================
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.



%% ====================================================================
%% Func: handle_cast/2
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private 
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.



%% ====================================================================
%% Func: handle_info/2
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private 
%% ====================================================================
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state
                {   listener=ListSock,
                    acceptor=Ref,
                    module=Module
                } = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
            ok              -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,
        %% New client connected - spawn a new process using the
        %% simple_one_for_one supervisor.
        {ok, Pid} = tcp_server_app:start_client(),
        gen_tcp:controlling_process(CliSocket, Pid),
        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, CliSocket),
        %% Signal the network driver that we are ready to accept
        %% another connection
        case prim_inet:async_accept(ListSock, -1) of
            {ok,    NewRef} -> ok;
            {error, NewRef} -> exit
                (
                    {   async_accept,
                        inet:format_error(NewRef)
                    }
                )
        end,
        {noreply, State#state{acceptor = NewRef}}
    catch exit:Why ->
        error_logger:error_msg
            (   "Error in async accept: ~p.\n",
                [Why]
            ),
        {stop, Why, State}
    end;
handle_info({   inet_async,
                ListSock,
                Ref,
                Error
            },
            #state
                {   listener = ListSock,
                    acceptor = Ref
                } = State
           ) ->
    error_logger:error_msg
        (   "Error in socket acceptor: ~p.\n",
            [Error]
        ),
    {stop, Error, State};
handle_info(_Info, State) ->
    {noreply, State}.



%% ====================================================================
%% Func: terminate/2
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%% ====================================================================
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.



%% ====================================================================
%% Func: code_change/3
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ====================================================================
%% Internal functions
%% Taken from prim_inet.  We are merely copying some socket options
%% from the listening socket to the new client socket.
%% ====================================================================
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock,
                            [   active,
                                nodelay,
                                keepalive,
                                delay_send,
                                priority,
                                tos
                            ]
                          ) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close(CliSocket), Error
            end;
        Error ->
            gen_tcp:close(CliSocket), Error
    end.

