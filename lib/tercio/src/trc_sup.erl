%%%-------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%%  Tercio core supervisor.
%%% @end
%%% @copyright (C) 2006
%%% Created : 18 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(trc_sup).

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
    Tercio = {tercio,{tercio,start_link,[]},
              permanent,2000,worker,[tercio]},
    {ok,{{one_for_all,0,1}, [Tercio]}}.

%%====================================================================
%% Internal functions
%%====================================================================
