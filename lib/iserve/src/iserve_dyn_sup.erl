%%%-------------------------------------------------------------------
%%% File    : iserve_dyn_sup.erl
%%% Author  : emerritt <>
%%% Description : 
%%%
%%% Created : 28 Aug 2006 by emerritt <>
%%%-------------------------------------------------------------------
-module(iserve_dyn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, 
        new_server/2]).

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

%%--------------------------------------------------------------------
%% @spec new_server(Port, CallBack) -> ok.
%% 
%% @doc 
%%  Start the new server with the specified port and the specified 
%%  callbacks.
%% @end
%%--------------------------------------------------------------------
new_server(Port, CallBack) ->
    supervisor:start_child(?SERVER, [Port, CallBack]).

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
    AChild = {iserve_server,{iserve_server, start_link, []},
              permanent, 2000, worker,[iserve_server]},
    {ok,{{simple_one_for_one,0,1}, [AChild]}}.






%%====================================================================
%% Internal functions
%%====================================================================
