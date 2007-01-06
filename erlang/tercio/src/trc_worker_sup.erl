%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%   Support for tercio workers. Workers are transiant so this doesn
%%% @end
%%% @copyright (C) 2007
%%% Created :  5 Jan 2007 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(trc_worker_sup).

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
%% 
%% @doc 
%% Starts the supervisor
%% @end
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
%% 
%% @doc 
%%  Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    AChild = {worker,{trc_worker,start_link,[]},
              transient,2000,worker,[trc_worker]},
    {ok,{{simple_one_for_one,0,1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
