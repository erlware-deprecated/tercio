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
%%% 
%%% @end
%%% @copyright (C) 2006
%%% Created : 21 Dec 2006 by Eric Merritt
%%%--------------------------------------------------------------------------
-module(carre_app).


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
    case carre_sup:start_link() of
	{ok, Pid} -> 
	    alarm_handler:clear_alarm({application_stopped, carre}),
	    {ok, Pid};
	Error ->
	    alarm_handler:set_alarm({{application_stopped, carre},[]}),
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
    alarm_handler:set_alarm({{application_stopped, carre},[]}),
    ok.

%%====================================================================
%% Internal functions
%%===================================================================