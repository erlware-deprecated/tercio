%%%-------------------------------------------------------------------
%%% @author Eric Merrit
%%% @doc 
%%% Iserve system supervisor
%%% @end
%%% Created : 28 Aug 2006 by Eric Merritt
%%%-------------------------------------------------------------------
-module(iserve_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}.
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    DynSup = {iserve_dyn_sup, {iserve_dyn_sup, start_link,[]},
              permanent, 2000, supervisor,[iserve_dyn_sup, iserve_server]},
    Iserve = {iserve, {iserve, start_link,[]},
              permanent,2000,worker,[iserve, iserve_server]},
    {ok,{{one_for_one,0,1}, [DynSup, Iserve]}}.

%%====================================================================
%% Internal functions
%%====================================================================
