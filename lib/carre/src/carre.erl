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
%%% @author Eric Merritt
%%% @doc 
%%%  Serves as a top level control function for an carre subsystem
%%% @end
%%% @copyright
%%%--------------------------------------------------------------------------
-module(carre).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0,
         start_server/1,
         start_server/2,
         register_handler/1,
         unregister_handler/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-define(DEFAULT_SESSION, 30 * 60 * 1000).

-record(state, {handlers}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% @doc
%%   Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc 
%%  Register a new handler for this system.
%% @spec register_handler(Handler) -> ok.
%% @end
%%--------------------------------------------------------------------
register_handler(Handler) ->
    gen_server:cast(?SERVER, {register_handler, Handler}).

%%--------------------------------------------------------------------
%% @doc 
%%  unregister the handler
%% @spec unregister_handler(Handler) -> ok.
%% @end
%%--------------------------------------------------------------------
unregister_handler(Handler) ->
    gen_server:cast(?SERVER, {unregister_handler, Handler}).


%%--------------------------------------------------------------------
%% @spec start_server(Port::integer()) -> ok.
%%
%% @doc
%%  Starts a specific server on the specified port
%% @end
%%--------------------------------------------------------------------
start_server(Port) ->
    gen_server:cast(?SERVER, {newserver, Port, ?DEFAULT_SESSION}).

%%--------------------------------------------------------------------
%% @spec start_server(Port::integer(), Session::integer()) -> ok.
%%
%% @doc
%%  Starts a specific server on the specified port
%% @end
%%--------------------------------------------------------------------
start_server(Port, Session) ->
    gen_server:cast(?SERVER, {newserver, Port, Session}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}.
%% @doc Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{handlers=[]}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%% @doc Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% @doc Handling cast messages
%%--------------------------------------------------------------------
handle_cast({newserver, Port, Session}, State = #state{handlers=Handlers}) ->
    carre_dyn_sup:new_server(Port, Session, Handlers),
    {noreply, State};
handle_cast({register_handler, Handler}, #state{handlers=Handlers}) ->
    {noreply, #state{handlers=[Handler | Handlers]}};
handle_cast({unregister_handler, Handler}, #state{handlers=Handlers}) ->
    {noreply, #state{handlers=strip_handler(Handler, Handlers, [])}}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% @doc Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
%% 
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% 
%% @doc Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec strip_handler(Handler, List, Acc) -> NewList.
%% 
%% @doc strip the handler from the handler list.
%%--------------------------------------------------------------------
strip_handler(Handler, [Handler | T], Acc) ->
    Acc ++ T;
strip_handler(Handler, [H | T], Acc) ->
    strip_handler(Handler, T, [H | Acc]);
strip_handler(_Handler, [], Acc) ->
    Acc.
