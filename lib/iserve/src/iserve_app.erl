%%%-------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%%  I serve application interface.
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(iserve_app).

-behaviour(application).

-export([
	 start/2,
	 stop/1
        ]).

%%--------------------------------------------------------------------
%% @spec start(_Type, _StartArgs) -> ok.
%% 
%% @doc 
%%  Start up the iserve app in the normal manner.
%% @end
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case iserve_sup:start_link() of
	{ok, Pid} -> 
	    alarm_handler:clear_alarm({application_stopped, iserve}),
	    {ok, Pid};
	Error ->
	    alarm_handler:set_alarm({{application_stopped, iserve},[]}),
	    Error
    end.

%%--------------------------------------------------------------------
%% @spec stop(_State) -> ok.
%% 
%% @doc 
%%  stop the application
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    alarm_handler:set_alarm({{application_stopped, iserve},[]}),
    ok.


