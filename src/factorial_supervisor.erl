%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo factorial_supervisor


-module(factorial_supervisor).
-author("kuangyel2000@gmail.com").

-behaviour(supervisor).



-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_link_shell/0]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
start_link_shell() ->
    {ok, Pid} = supervisor:start_link({global, ?MODULE}, ?MODULE, []),
    unlink(Pid).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).



%% ====================================================================
%% init/1 functions
%% ====================================================================
init([]) ->
    io:format("~p (~p) start ... ~n", [{global, ?MODULE}, self()]),
    %% If MaxRestarts restarts occur in MaxSecondsBetweenRestarts seconds
    %% supervisor and child processes are killed
    RestartStrategy = one_for_one,
    MaxRestart = 3,                 %% 3 restart within
    MaxSecondsBetweenRestarts = 5,  %% five seconds
    Flags = {RestartStrategy, MaxRestart, MaxSecondsBetweenRestarts},
    %% permanent - always restart
    %% temporary - never restart
    %% transient - restart if abnormally ends
    Restart = permanent,
    %% burtal_kill - use exit(Child, kill) to terminate
    %% integer - use exit(Child, shutdown) - milliseconds
    Shutdown = infinity,
    %% worker
    %% supervisor
    Type = worker,
    %% Modules ones supervisor uses
    %% {ChildId, {StartFunc = {module, function, arg}, Restart, Shutdown, Type, Modules}}.
    ChildSpecifications = {factorialServerId, {factorial_server, start_link, []}, Restart, Shutdown, Type, [factorial_server]},
    %% tuple of restart strategy, max restart and max time
    %% child specification
    {ok, {Flags, [ChildSpecifications]}}.

