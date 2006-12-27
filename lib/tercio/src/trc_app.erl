%%%-------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc 
%%%  Support for the tercio application.
%%% @end
%%% @copyright (C) 2006
%%% Created : 18 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(trc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}.
%% 
%% @doc 
%% This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case start_app() of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @spec stop(State) -> void().
%% 
%% @doc 
%% This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%% @end 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_app() ->
    start_server(),
    trc_sup:start_link().

start_server() ->
    case tconfig:get_value("HttpServer.Type") of
        "internal" ->
            carre:start();
        "Else" ->
            ok
    end.
