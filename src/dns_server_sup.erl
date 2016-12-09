%%%-------------------------------------------------------------------
%% @doc dns_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dns_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->

    % Supervisor specification 
    %
    Restart_strategy = one_for_one,
    Max_restarts = 1,                  % one restart every
    Max_seconds_between_restarts = 5,  % five seconds
    Supervisor_spec = {Restart_strategy, 
                       Max_restarts, 
                       Max_seconds_between_restarts},

    Restart = permanent, % permanent or temporary or transient
    Shutdown = 2000,     % milliseconds
    Type = worker, 

    % Configuration
    %
    {ok,Application} = application:get_env(dns_application),
    {ok,FileName} = application:get_env(dns_filename),
    {ok,Port} = application:get_env(dns_port),
    File = filename:join(code:priv_dir(Application)++"/",FileName),

    % Child specification
    %
    Child_spec = {dns_server, % id           
                  {dns_server,dns_start, [File,Port]}, % module,start func,args
                  Restart, 
                  Shutdown, 
                  Type, 
                  [dns_server,dns_db_server]}, % module list
    {ok, {Supervisor_spec, [Child_spec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
