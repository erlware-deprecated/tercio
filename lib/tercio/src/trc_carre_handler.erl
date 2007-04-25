%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%  Implements handler semantics for tercios interaction with carre.
%%%  for the most part this just to serve relevant javascript.
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 21 Apr 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(trc_carre_handler).

-behaviour(carre_handler).

-export([handle/3, setup/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  Handle the incoming request. Make sure to handle only what 
%%  you need to handle.
%% @spec handle(Handle, Req, Id) -> CarreResp.
%% @todo Cache the file, don't read from disc every time
%% @end
%%--------------------------------------------------------------------
handle(["/", "tercio", Name], Req, Id) ->
    Dir = code:priv_dir(tercio),
    TargetFile = filename:join([Dir, Name]),
    carre_utils:process_file(TargetFile).


%%--------------------------------------------------------------------
%% @doc 
%%  Register this handler with the carre process. 
%% @spec setup() -> ok.
%% @end
%%--------------------------------------------------------------------
setup() ->
    carre:register_handler(?MODULE).
