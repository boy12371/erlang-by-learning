%% @author Richard
%% @doc @todo In order to build an OTP application we need to construct
%% modules implementing an application and supervisor behaviour
%% callback functions. While traditionally these functionalities are
%% implemented in separate modules, given their succinctness we'll
%% combine them in one module.


-module(tcp_server_app).
-author("kuangyel2000@gmail.com").

-behaviour(application).



%% ====================================================================
%% Internal API
%% ====================================================================
-export([start_client/0]).



%% ====================================================================
%% Application and Supervisor callbacks
%% ====================================================================
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    8086).



%% ====================================================================
%% start_client/0 functions
%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
%% ====================================================================
start_client() ->
    supervisor:start_child(tcp_client_sup, []).


%% ====================================================================
%% Application behaviour callbacks
%% ====================================================================
start(_Type, _Args) ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link(  {local, ?MODULE},
                            ?MODULE,
                            [ListenPort, tcp_echo_fsm]
                         ).

stop(_S) ->
    ok.



%% ====================================================================
%% Supervisor behaviour callbacks
%% ====================================================================
init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
                % TCP Listener
                    % Id       = internal id
                {   tcp_server_sup,
                    % StartFun = {M, F, A}
                    {tcp_listener, start_link, [Port,Module]},
                    % Restart  = permanent | transient | temporary
                    permanent,
                    % Shutdown = brutal_kill | int() >= 0 | infinity
                    2000,
                    % Type     = worker | supervisor
                    worker,
                    % Modules  = [Module] | dynamic
                    [tcp_listener]
                },
                % Client instance supervisor
                {   tcp_client_sup,
                    {   supervisor,
                        start_link,
                        [   {local, tcp_client_sup},
                            ?MODULE,
                            [Module]
                        ]
                    },
                    % Restart  = permanent | transient | temporary
                    permanent,
                    % Shutdown = brutal_kill | int() >= 0 | infinity
                    infinity,
                    % Type     = worker | supervisor
                    supervisor,
                    % Modules  = [Module] | dynamic
                    []
                }
            ]
        }
    };
init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
                % TCP Client
                    % Id       = internal id
                {   undefined,
                    % StartFun = {M, F, A}
                    {Module, start_link, []},
                    % Restart  = permanent | transient | temporary
                    temporary,
                    % Shutdown = brutal_kill | int() >= 0 | infinity
                    2000,
                    % Type     = worker | supervisor
                    worker,
                    % Modules  = [Module] | dynamic
                    []
                }
            ]
        }
    }.



%% ====================================================================
%% Internal functions
%% ====================================================================
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
        {ok, Val} ->
            Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] ->
                    Val;
                error       ->
                    Default
            end
    end.

