%%%-------------------------------------------------------------------
%%% Copyright 2006 Eric Merritt
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");  
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%  Unless required by applicable law or agreed to in writing, software
%%%  distributed under the License is distributed on an "AS IS" BASIS,
%%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or 
%%%  implied. See the License for the specific language governing 
%%%  permissions and limitations under the License.
%%%
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  Supervises the dynamically created http servers. 
%%% @end
%%% Created : 28 Aug 2006 by emerritt <>
%%%--------------------------------------------------------------------------
-module(carre_dyn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, 
        new_server/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% @end Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}.
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Server = {carre_server,{carre_server, start_link, []},
              temporary, 2000, worker,[carre_server]},
    {ok,{{simple_one_for_one,0,1}, [Server]}}.


%%--------------------------------------------------------------------
%% @spec new_server(Port) -> Pid.
%% 
%% @doc 
%%  Start a new server (using the simple_one_for_one).
%% @end
%%--------------------------------------------------------------------
new_server(Port, Session, Handlers) ->
    supervisor:start_child(?SERVER, [Port, Session, Handlers]).

%%====================================================================
%% Internal functions
%%====================================================================
