%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%%
%%% @doc
%%%  Handles the carre listening process.
%%% @end
%%%-------------------------------------------------------------------
-module(carre_server).

-behaviour(gen_server).

-export([start_link/1, 
         create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {listen_socket,
                port,
                acceptor,
                server_root,
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
start_link(Port) when is_integer(Port) ->
    Name = list_to_atom(lists:flatten(io_lib:format("carre_~w",[Port]))),
    gen_server:start_link({local, Name}, ?MODULE, [Port], []).

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
init([Port]) ->
    process_flag(trap_exit, true),
    
    ServerRoot = tconfig:get_value("ServerRoot"),
    Session = tconfig:get_value("Session.Duration"),

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
                                          filename:join([ServerRoot, 
                                                         "public"])),
	    {ok, #state{listen_socket = Listen_socket,
                        port = Port,
			acceptor = Pid,
                        server_root=ServerRoot,
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
                                          server_root=ServerRoot,
                                          session=Session}) ->
    New_pid = carre_socket:start_link(self(), Listen_socket, 
                                      Port,
                                      ServerRoot,
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
                                        server_root = ServerRoot,
                                        session=Session}) ->
    timer:sleep(2000),
    carre_socket:start_link(self(), ListenSocket, 
                            Port, ServerRoot, Session),
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
