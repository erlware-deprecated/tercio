%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%   Top level supervisor for the carre app.
%%% @end
%%%-------------------------------------------------------------------
-module(carre_sup).

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
    DynSup = {carre_dyn_sup, {carre_dyn_sup, start_link,[]},
              permanent, 2000, supervisor,[carre_dyn_sup, carre_server]},
    Carre = {carre, {carre, start_link,[]},
              permanent,2000,worker,[carre, carre_server]},
    {ok,{{one_for_one,0,1}, [DynSup, Carre]}}.

%%====================================================================
%% Internal functions
%%====================================================================
