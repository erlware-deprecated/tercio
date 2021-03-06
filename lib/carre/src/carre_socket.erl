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
%%% @author Sean Hinde <seanhinde@users.sourceforge.net>
%%% @author Eric Merritt <cyberlync@gmail.com>
%%%
%%% @doc
%%%  Provides protocol parsing and handler forwording on a per
%%%  connection basis. In a small reverse of the common approach
%%%  this process does the listening as well. It forwards the acceptor
%%%  socket back to the server before contining on with the accepted
%%%  socket. This provides for reduced probability of timing bugs
%%%  with very little additional conceptual overhead. The parser 
%%%  makes use of the built in ability of erlang sockets to handle
%%%  http and https protocol parsing. This results in some limitation
%%%  of the total size of incoming http packets but overall its a 
%%%  worthwile trade off in a small demonstration server like this.
%%% @end 
%%%---------------------------------------------------------------------------
-module(carre_socket).

-export([start_link/5]).

-export([init/5]).

-include("carre.hrl").

-record(c,  {sock,
             port,
             peer_addr,
             peer_port,
             handlers,
             session}).

-define(server_idle_timeout, 30*1000).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(ListenPid, ListenSocket, ListenPort, 
%%                  Handlers, Session) -> ok.
%%
%% 
%% @doc 
%%  Start the socket server listening process. 
%% @end
%%--------------------------------------------------------------------
start_link(ListenPid, ListenSocket, ListenPort, 
           Handlers, Session) ->
    proc_lib:spawn_link(?MODULE, init, [ListenPid, ListenSocket, 
                                        ListenPort, Handlers, Session]).

%%--------------------------------------------------------------------
%% @spec init(Listen_pid, Listen_socket, ListenPort, 
%%   Handlers, Session) -> ok.
%% 
%% @doc 
%%   Initiate the listening process.
%% @end
%%--------------------------------------------------------------------
init(Listen_pid, Listen_socket, ListenPort, Handlers, Session) ->
    case catch gen_tcp:accept(Listen_socket) of
	{ok, Socket} ->
            %% Send the cast message to the listener process to 
            %% create a new acceptor
	    carre_server:create(Listen_pid, self()),
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #c{sock = Socket,
                   port = ListenPort,
                   peer_addr = Addr,
                   peer_port = Port,
                   handlers=Handlers,
                   session=Session},
	    request(C, #req{}); %% Jump to state 'request'
	Else ->
	    error_logger:error_report([{application, carre},
				       "Accept failed error",
				       io_lib:format("~p",[Else])]),
	    exit({error, accept_failed})
    end.


%%====================================================================
%% Internal functions
%%====================================================================


request(C = #c{sock=Sock}, Req) ->
    case gen_tcp:recv(Sock, 0, 30000) of
        {ok, {http_request,Method,Path,Version}} ->
            headers(C, Req#req{ sock = Sock,
                                vsn = Version,
                                method = Method,
                                uri = Path}, []);
        {error, {http_error, "\r\n"}} ->
	    request(C, Req);
	{error, {http_error, "\n"}} ->
            request(C, Req);
	_Other ->
	    exit(normal)
    end.

headers(C, Req, H) ->
    case gen_tcp:recv(C#c.sock, 0, ?server_idle_timeout) of
        {ok, {http_header,_,'Content-Length',_,Val}} ->
            Len = list_to_integer(Val),
            headers(C, Req#req{content_length = Len}, 
                    [{'Content-Length', Len}|H]);
        {ok, {http_header,_,'Connection',_,Val}} ->
            Keep_alive = keep_alive(Req#req.vsn, Val),
            headers(C, Req#req{connection = Keep_alive}, 
                    [{'Connection', Val}|H]);
        {ok, {http_header,_,Header,_,Val}} ->
            headers(C, Req, [{Header, Val}|H]);
        {error, {http_error, "\r\n"}} ->
	    headers(C, Req, H);
	{error, {http_error, "\n"}} ->
            headers(C, Req, H);
        {ok, http_eoh} ->
            body(C, Req#req{headers = lists:reverse(H)});
	_Other ->
	    exit(normal)
    end.

%% Shall we keep the connection alive? 
%% Default case for HTTP/1.1 is yes, default for HTTP/1.0 is no.
%% Exercise for the reader - finish this so it does case 
%% insensitivity properly !
keep_alive({1,1}, "close")      -> close;
keep_alive({1,1}, "Close")      -> close;
keep_alive({1,1}, _)            -> keep_alive;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive({1,0}, _)            -> close;
keep_alive({0,9}, _)            -> close;
keep_alive(_Vsn, _KA) ->
    close.

body(#c{sock = Sock} = C, Req) ->
    case Req#req.method of
        'GET' ->
            Close = handle_get(C, Req),
            case Close of
                close ->
                    gen_tcp:close(Sock);
                keep_alive ->
                    inet:setopts(Sock, [{packet, http}]),
                    request(C, #req{})
            end;
        'POST' when is_integer(Req#req.content_length) ->
            inet:setopts(Sock, [{packet, raw}]),
            case gen_tcp:recv(Sock, Req#req.content_length, 60000) of
                {ok, Bin} ->
                    Close = handle_post(C, Req#req{body = Bin}),
                    case Close of
                        close ->
                            gen_tcp:close(Sock);
                        keep_alive ->
                            inet:setopts(Sock, [{packet, http}]),
                            request(C, #req{})
                    end;
                _Other ->
                    exit(normal)
            end;
        _Other ->
            send(C, ?not_implemented_501),
            exit(normal)
    end.

handle_get(C = #c{handlers=Handlers,
                  session=Session}, Req = #req{connection = Conn}) ->
    case Req#req.uri of
        {abs_path, Path} ->
            {NPath, Args} = split_at_q_mark(Path, []),
            carre_dispatcher:do_get(Req#req{uri=NPath, args=Args}, 
                                 Handlers, 
                                 Session),
            Conn;
        {absoluteURI,http,_Host,_, Path} ->
            {NPath, Args} = split_at_q_mark(Path, []),
            carre_dispatcher:do_get(Req#req{uri=NPath, args=Args}, Handlers,
                                 Session),
            Conn;
        {absoluteURI,_Other_method,_Host,_,_Path} ->
            send(C, ?not_implemented_501),
            close;
        {scheme, _Scheme, _RequestString} ->
            send(C, ?not_implemented_501),
            close;
        _  ->
            send(C, ?forbidden_403),
            close
    end.

%%--------------------------------------------------------------------
%% @spec handle_post(C, Req) -> ok.
%% 
%% @doc 
%%  Parse post requests from the system.
%% @end
%%--------------------------------------------------------------------
handle_post(C = #c{handlers=Handlers,
                   session=Session}, Req = #req{connection = Conn}) ->
    case Req#req.uri of
        {abs_path, _Path} ->
            carre_dispatcher:do_post(Req, Handlers, Session),
            Conn;
        {absoluteURI, http, _Host, _Port, _Path} ->
            carre_dispatcher:do_post(Req, Handlers, Session),
            Conn;
        {absoluteURI, _Other_method, _Host, _Port, _Path} ->
            send(C, ?not_implemented_501),
            close;
        {scheme, _Scheme, _RequestString} ->
            send(C, ?not_implemented_501),
            close;
        _  ->
            send(C, ?forbidden_403),
            close
    end.

  
send(#c{sock = Sock}, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.


%% Split the path at the ?. This would have to do all sorts of
%% horrible ../../ path checks and %C3 etc decoding if we wanted to
%% retrieve actual paths to real filesystem files. As it is we only
%% want to look it up as a key in mnesia/ets :)
split_at_q_mark([$?|T], Acc) ->
    {lists:reverse(Acc), parse_query(T, [], [], [])};
split_at_q_mark([$%, Hi, Lo | T], Acc) ->
    split_at_q_mark(T, [hex_to_integer([Hi, Lo]) |Acc]);
split_at_q_mark([H|T], Acc) ->
    split_at_q_mark(T, [H|Acc]);
split_at_q_mark([], Acc) ->
    {lists:reverse(Acc), []}.


parse_query([$= | T], _KeyAcc, Buffer, Acc) ->
    parse_query(T, lists:reverse(Buffer), [], Acc);
parse_query([$& | T], KeyAcc, Buffer, Acc) ->
    parse_query(T, [], [], [{KeyAcc, lists:reverse(Buffer)} | Acc]);
parse_query([$%, Hi, Lo | T], KeyAcc, Buffer, Acc) ->
    parse_query(T, KeyAcc, [hex_to_integer([Hi, Lo]) | Buffer], Acc);             
parse_query([H | T], KeyAcc, Buffer, Acc) ->
    parse_query(T, KeyAcc, [H|Buffer], Acc);
parse_query([], KeyAcc, Buffer, Acc) ->
    [{KeyAcc, lists:reverse(Buffer)} | Acc].

%% hex_to_integer
                                                                                
hex_to_integer(Hex) ->
    case catch erlang:list_to_integer(Hex, 16) of
        {'EXIT', _} ->
            backup_hex_to_integer(Hex);
        X ->
            X
    end.

backup_hex_to_integer(Hex) ->
    DEHEX = fun (H) when H >= $a, H =< $f -> H - $a + 10;
                (H) when H >= $A, H =< $F -> H - $A + 10;
                (H) when H >= $0, H =< $9 -> H - $0
            end,
    lists:foldl(fun(E, Acc) -> Acc*16+DEHEX(E) end, 0, Hex).
