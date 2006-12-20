%%%-------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%%  Handles the iserve listening process.
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(iserve_server).

-behaviour(gen_server).

-export([start_link/2, 
         create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {listen_socket,
                port,
                acceptor,
                callback}).

%%====================================================================
%% API
%%====================================================================
start_link(Port, Callback) when is_integer(Port) ->
    Name = list_to_atom(lists:flatten(io_lib:format("iserve_~w",[Port]))),
    gen_server:start_link({local, Name}, ?MODULE, [Port, Callback], []).

%% Send message to cause a new acceptor to be created
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% Called by gen_server framework at process startup. Create listening socket
init([Port, CallBack]) ->
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
	    Pid = iserve_socket:start_link(self(), Listen_socket, 
                                           Port, CallBack),
	    {ok, #state{listen_socket = Listen_socket,
                        port = Port,
			acceptor = Pid,
                        callback = CallBack}};
	{error, Reason} ->
	    {stop, Reason}
    end.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%% Called by gen_server framework when the cast message from 
%% create/2 is received
handle_cast({create,_Pid},#state{listen_socket = Listen_socket,
                                 port = Port,
                                callback = CallBack} = State) ->
    New_pid = iserve_socket:start_link(self(), Listen_socket, 
                                       Port, CallBack),
    {noreply, State#state{acceptor=New_pid}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, normal}, #state{acceptor=Pid} = State) ->
    {noreply, State};

%% The current acceptor has died, wait a little and try again
handle_info({'EXIT', Pid, _Abnormal}, #state{listen_socket = ListenSocket,
                                             acceptor = Pid, 
                                             port = Port,
                                             callback = CallBack} = State) ->
    timer:sleep(2000),
    iserve_socket:start_link(self(), ListenSocket, 
                             Port, 
                             CallBack),
    {noreply,State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
