%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%  Support for handler behaviors for carre.
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 20 Apr 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(carre_handler).

-export([behaviour_info/1]).

%%====================================================================
%% API
%%====================================================================
behaviour_info(callbacks) ->
    [{handle, 3}];
behaviour_info(_Other) ->
    undefined.
