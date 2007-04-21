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
%%%
%%% @doc
%%%  Handles the carre listening process.
%%% @end
%%%---------------------------------------------------------------------------
-module(carre_server).

-behaviour(gen_server).

-export([start_link/3, 
         create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {listen_socket,
                port,
                acceptor,
                handlers,
                session}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% 
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link(Port, Session, Handlers) when is_integer(Port) ->
    gen_server:start_link(?MODULE, [Port, Session, Handlers], []).

%%--------------------------------------------------------------------
%% @spec create(ServerPid, Pid) -> ok.
%%
%% @doc
%%  Send message to cause a new acceptor to be created
%% @end
%%--------------------------------------------------------------------
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}.
%% 
%% @doc 
%% Called by gen_server framework at process startup. Create
%%  listening socket.
%% @end 
%%--------------------------------------------------------------------
init([Port, Session, Handlers]) ->
    process_flag(trap_exit, true),
    
    case gen_tcp:listen(Port,[binary,{packet,0},
                              {reuseaddr,true},
                              {active, false},
			      {packet, http},
                              {recbuf, 8192},
                              {packet_size, 8192},
                              {backlog, 30}]) of
	{ok, Listen_socket} ->
            %%Create first accepting process
	    Pid = carre_socket:start_link(self(), Listen_socket, 
                                          Port, 
                                          Handlers,
                                          Session),
	    {ok, #state{listen_socket = Listen_socket,
                        port = Port,
			acceptor = Pid,
                        handlers=Handlers,
                        session=Session}};
	{error, Reason} ->
	    {stop, Reason}
    end.


%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Called by gen_server framework when the cast message from 
%% create/2 is received
%% @end 
%%--------------------------------------------------------------------
handle_cast({create,_Pid}, State = #state{listen_socket = Listen_socket,
                                          port = Port,
                                          handlers = Handlers,
                                          session=Session}) ->
    New_pid = carre_socket:start_link(self(), Listen_socket, 
                                      Port,
                                      Handlers,
                                      Session),
    {noreply, State#state{acceptor=New_pid}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, normal}, #state{acceptor=Pid} = State) ->
    {noreply, State};

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% 
%% @doc 
%% The current acceptor has died, wait a little and try again
%% @end 
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _Abnormal}, State = #state{
                                        listen_socket = ListenSocket,
                                        acceptor = Pid, 
                                        port = Port,
                                        handlers = Handlers,
                                        session=Session}) ->
    timer:sleep(2000),
    carre_socket:start_link(self(), ListenSocket, 
                            Port, Handlers, Session),
    {noreply,State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
%% 
%% @doc 
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% 
%% @doc 
%% Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
