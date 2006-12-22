%%%-------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc 
%%%  Serves as a top level control function for an carre subsystem
%%% @end
%%% @copyright
%%%-------------------------------------------------------------------
-module(carre).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0,
        start_server/2]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(state, {}).

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
%% @spec start_server(Port::integer(), Callback::function()) -> ok.
%%
%% @doc
%%  Starts a specific server on the specified port, using the 
%%  specified callback as a handler.
%% @end
%%--------------------------------------------------------------------
start_server(Port, CallBack) ->
    gen_server:cast(?SERVER, {newserver, Port, CallBack}).

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
    {ok, #state{}}.

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
handle_cast({newserver, Port, CallBack}, State) ->
    carre_dyn_sup:new_server(Port, CallBack),
    {noreply, State}.

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
