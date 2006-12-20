%%%-------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Client api for the iserve server
%%% @end
%%%
%%% @copyright 2006
%%%-------------------------------------------------------------------
-module(iserve_client).



-export([respond/2, respond/3, respond/4]).


-include("iserve.hrl").

%%====================================================================
%% API functions
%%====================================================================
%%%-------------------------------------------------------------------
%% @spec respond(C::record(req), Body::io_list()) -> ok.
%%
%% @doc
%% Responds to the request. 
%% @end
%%%-------------------------------------------------------------------
respond(C, Body) ->
    respond(C, "200 OK", [], Body).

%%%-------------------------------------------------------------------
%% @spec respond(C::record(req), Type::string(), Body::io_list()) -> ok.
%%
%% @doc
%% Responds to the request. Type is expected to be an http
%% type like "200 OK"
%% @end
%%%-------------------------------------------------------------------
respond(C, Type, Body) ->
    respond(C, Type, [], Body).

%%%-------------------------------------------------------------------
%% @spec respond(C::record(req), Type::string(), Headers::list(),
%%   Body::io_list()) -> ok.
%%
%% @doc
%% Responds to the request. Type is expected to be an http
%% type like "200 OK", Headers is a list of {Tag, Value}
%% tuples
%% @end
%%%-------------------------------------------------------------------
respond(Req, Type, Headers, Body) ->
    NewBody = conv_to_binary(Body),
    NewHeaders = add_content_length(Headers, NewBody),
    Enc_headers = enc_headers(NewHeaders),
    Resp = [<<"HTTP/1.1 ">>, Type, <<"\r\n">>,
            Enc_headers,
            <<"\r\n">>,
            NewBody],
    send(Req, Resp).


%%====================================================================
%% Internal functions
%%====================================================================
conv_to_binary(Val) when is_binary(Val) ->
    Val;
conv_to_binary(Val) when is_list(Val) ->
    list_to_binary(Val).


add_content_length(Headers, Body) ->
    case lists:keysearch('Content-Length', 1, Headers) of
        {value, _} ->
            Headers;
        false ->
            [{'Content-Length', size(Body)}|Headers]
    end.


enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
    [atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
    [Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([]) ->
    [].

enc_header_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
enc_header_val(Val) ->
    Val.

send(#req{sock = Sock}, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.
