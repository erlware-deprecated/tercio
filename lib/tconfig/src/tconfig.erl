%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%% 
%%% @end
%%% @copyright (C) 2006
%%% Created : 21 Dec 2006 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(tconfig).

-behaviour(gen_server).

-include("eunit.hrl").

%% API
-export([start_link/0, get_value/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {config}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @spec get_value(Key) -> ok.
%% 
%% @doc 
%%  get a value from the config. Configs consist of nested proplists
%%  to access a deeply nested proplist you can either use a dotted
%%  notation like "X.Y.Z" or a a tuple list like {keypath, ["X", 
%%  "Y", "Z"]}
%% @end
%%--------------------------------------------------------------------
get_value(Key) when is_list(Key) ->
    gen_server:call(?SERVER, {get, tuplize(Key, [], [])});
get_value({keypath, Key}) when is_tuple(Key) ->
    gen_server:call(?SERVER, {get, Key}).


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
%% Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    Root = server_root(),
    Env = get_run_env(),
    {ok, #state{config=parse_config(Root, Env)}}.

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
handle_call({get, Tuple}, _From, State = #state{config=Config}) ->
    {reply, get_item(Tuple, Config), State};
handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% 
%% @doc 
%% Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
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
terminate(_Reason, _State) ->
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

%%====================================================================
%%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec get_run_env() -> Environment.
%% 
%% @doc 
%%  Get the runtime config environment.
%% @end
%%--------------------------------------------------------------------
get_run_env() ->
    case application:get_env(tconfig, environment) of
        undefined ->
            "development";
        {ok, Else} ->
            Else
    end.

%%--------------------------------------------------------------------
%% @spec server_root() -> Root.
%% 
%% @doc 
%%  Get the server root from the system. Exit if the root isn't 
%%  available.
%% @end
%%--------------------------------------------------------------------
server_root() ->
    case application:get_env(tconfig, server_root) of
        undefined ->
            error_logger:error_msg("Unable to get server root from the "
                                   "system. The server was not started "
                                   "correctly. Exiting "),
            exit({error, no_server_root});
        {ok, Root} ->
            Root
    end.


%%--------------------------------------------------------------------
%% @spec parse_config(Root, Env) -> ParsedConfig.
%% 
%% @doc 
%%  Read in the correct config file. Root specifies server root and 
%%  env specifies the runtime environment.
%% @end
%%--------------------------------------------------------------------
parse_config(Root, Env) ->
    ConfigFile = filename:join([Root, "config", 
                                string:concat(Env, ".config")]),
    case file:read_file(ConfigFile) of
        {ok, FileBin} ->
            parse_config(binary_to_list(FileBin));
        {error, Reason} ->
            error_logger:error_msg("Unable to read config file (~s). "
                                   "received ~w", [ConfigFile, Reason]),
            exit({error, unable_to_read_config})
    end.


%%--------------------------------------------------------------------
%% @spec parse_config(Stream) -> ParsedConfig.
%% 
%% @doc 
%%  Parse the config  file into a usable format.
%% @end
%%--------------------------------------------------------------------
parse_config([$\s | T]) ->
    parse_config(T);
parse_config([$\t | T]) ->
    parse_config(T);
parse_config([$\n | T]) ->
    parse_config(T);
parse_config([$\r | T]) ->
    parse_config(T);
parse_config(All = [${ | _]) ->
    {Value, _} = tcomm_json:decode(All),
    Value;
parse_config(All) ->
    {Value, _} = tcomm_json:decode([${ | All] ++ [$}]),
    Value.

tuplize([$. | T], TAcc, Acc) ->
    tuplize(T, [], [lists:reverse(TAcc) | Acc]);
tuplize([H | T], TAcc, Acc) ->
    tuplize(T, [H | TAcc], Acc);
tuplize([], [], Acc) ->
    lists:reverse(Acc);
tuplize([], TAcc, Acc) ->
    lists:reverse([lists:reverse(TAcc) | Acc]).


%%--------------------------------------------------------------------
%% @spec get_item(Key, Config) -> Value.
%% 
%% @doc 
%%  Get the item from the system identified by the key path.
%% @end
%%--------------------------------------------------------------------
get_item(_Key, undefined) ->
    undefined;
get_item([H | T], Config) ->
    get_item(T, get(H, Config));
get_item([], Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec get(Item, List) -> Value.
%% 
%% @doc 
%%  Get the specified item from the prop list.
%% @end
%%--------------------------------------------------------------------
get(Item, [{Item, Value} | _]) ->
    Value;
get(Item, [ _ | T]) ->
    get(Item, T);
get(_Item, []) ->
    undefined.

%%====================================================================
%%% Tests
%%====================================================================
tuplize_test() ->
    ?assertMatch({"Hello", "Hola"}, 
                 tuplize("Hello.Hola", [], [])),
    ?assertMatch({"One", "Two"},
                 tuplize("One.Two.", [], [])).

get_item_test() ->
    ?assertMatch(99, get_item(["Hello", "Port"],
                              [{"Boo", "Blah"},
                               {"Hello", [{"Pah", 100}, 
                                          {"Port", 99}]}])),
    ?assertMatch([{"Hello", "Goodbuy"}],
                 get_item(["Brody", "Brady", "Brah"],
                          [{"Boo", 100},
                           {"Brak", "Boo"},
                           {"Brody", [{"pooky", "pah"},
                                      {"Brady", [{"Brah", 
                                                  [{"Hello", 
                                                    "Goodbuy"}]}]}]}])).
