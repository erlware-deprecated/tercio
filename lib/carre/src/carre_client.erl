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
%%% @author Eric Merritt
%%% @doc
%%%  Client api for the carre server
%%% @end
%%%
%%% @copyright 2006 
%%%---------------------------------------------------------------------------
-module(carre_client).


-export([respond/2, respond/3, respond/4]).


-include("carre.hrl").

%%====================================================================
%% API functions
%%====================================================================
%%-------------------------------------------------------------------
%% @spec respond(C::record(req), Body::io_list) -> ok
%%
%% @doc
%% Responds to the request. 
%% @end
%%-------------------------------------------------------------------
respond(C, Body) ->
    respond(C, "200 OK", [], Body).

%%-------------------------------------------------------------------
%% @spec respond(C::record(req), Type::string, Body::io_list) -> ok
%%
%% @doc
%% Responds to the request. Type is expected to be an http
%% type like "200 OK"
%% @end
%%-------------------------------------------------------------------
respond(C, Type, Body) ->
    respond(C, Type, [], Body).

%%-------------------------------------------------------------------
%% @spec respond(C::record(req), Type::string(), Headers::list(),
%%   Body::io_list()) -> ok
%%
%% @doc
%% Responds to the request. Type is expected to be an http
%% type like "200 OK", Headers is a list of {Tag, Value}
%% tuples
%% @end
%%-------------------------------------------------------------------
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
