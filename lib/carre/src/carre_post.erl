%%%-------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc 
%%%   This module provides functionality for parsing post messages.
%%% @end
%%% @copyright (C) 2006
%%% Created : 27 Dec 2006 
%%%-------------------------------------------------------------------
-module(carre_post).

-export([parse/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec parse_post(Bin) -> [{Name, Value}, ...].
%% @doc
%% parse POST data when ENCTYPE is unset or
%% Content-type: application/x-www-form-urlencoded
%% Bin is the content of ARG#arg.clidata
%% the alternative is
%% Content-type: multipart/form-data; boundary=-------------------7cd1d6371ec
%% which is used for file upload 
%% @end
%%--------------------------------------------------------------------
parse(Bin) ->
    do_parse_spec(Bin, nokey, [], key).      


%%====================================================================
%% Internal Functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec do_parse_spec(Spec, Last, Cur, State) -> [{Name, Value}, ...].
%%
%% @doc 
%% It will return a [{Key, Value}] list from the post data
%% with the same length as the Spec or EXIT
%% special value undefined is reserverd for non set fields
%% Key wil always be a regular atom.
%% @end
do_parse_spec(<<$\%, Hi:8, Lo:8, Tail/binary>>, Last, Cur, State) ->
    Hex = yaws:hex_to_integer([Hi, Lo]),
    do_parse_spec(Tail,  Last, [ Hex | Cur],  State);
do_parse_spec(<<$&, Tail/binary>>,  _Last , Cur,  key) ->
    [{lists:reverse(Cur), undefined} |
     do_parse_spec(Tail, nokey, [], key)]; 
do_parse_spec(<<$&, Tail/binary>>, Last, Cur, value) ->
    V = {Last, lists:reverse(Cur)},
    [V | do_parse_spec(Tail, nokey, [], key)];
do_parse_spec(<<$+, Tail/binary>>, Last, Cur,  State) ->
    do_parse_spec(Tail, Last, [$\s|Cur], State);
do_parse_spec(<<$=, Tail/binary>>, _Last, Cur, key) ->
    do_parse_spec(Tail, lists:reverse(Cur), [], value);
do_parse_spec(<<H:8, Tail/binary>>, Last, Cur, State) ->
    do_parse_spec(Tail,  Last, [H|Cur], State);
do_parse_spec(<<>>, nokey, Cur, _State) ->
    [{lists:reverse(Cur), undefined}];
do_parse_spec(<<>>, Last, Cur, _State) ->
    [{Last, lists:reverse(Cur)}];
do_parse_spec(undefined,_,_,_) ->
    [];
do_parse_spec(QueryList, Last, Cur, State) when list(QueryList) ->
    do_parse_spec(list_to_binary(QueryList), Last, Cur, State).
