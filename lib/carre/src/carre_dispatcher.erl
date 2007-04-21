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
%%%   Handles the incoming parts of the system.  Parsing and forwarding 
%%%   on to the correct tercio services.
%%% @end
%%% @copyright (C) 2006
%%% Created : 26 Dec 2006 
%%%---------------------------------------------------------------------------
-module(carre_dispatcher).

-include("carre.hrl").

%% API
-export([do_get/3, do_post/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec do_get(Req, Handler, Session) ->
%% @doc
%%  Do the get requests. These are by definition requests for files.
%% @end 
%%--------------------------------------------------------------------
do_get(Req = #req{uri=Uri}, Handlers, Session) ->
    RFile = filename:split(Uri),
    case scan_for_problems(RFile) of
        problems ->
            carre_client:respond(Req, "403 Forbidden", "");
        ok ->
            case get_session_id(Req) of
                undefined ->
                    {Id, SessionCookie} = setup_session_cookie(Req, Session),
                    case dispatch(Handlers, RFile, Req, Id) of
                        {Headers, Data} ->
                            carre_client:respond(Req, "200 ok", 
                                                 [{"Content-Type",
                                                   "text/javascript"},
                                                  SessionCookie | Headers],
                                                 Data);
                        {Ret, Headers, Data} ->
                            carre_client:respond(Req, Ret, 
                                                 [{"Content-Type",
                                                   "text/javascript"},
                                                  SessionCookie | Headers],
                                                 Data)
                    end;
                  
                Id ->
                    case dispatch(Handlers, RFile, Req, Id) of 
                        {Headers, Data} ->
                            carre_client:respond(Req, "200 ok", 
                                                 [{"Content-Type",
                                                   "text/javascript"} | 
                                                  Headers],
                                                 Data);
                        {Ret, Headers, Data} ->
                            carre_client:respond(Req, Ret, 
                                                 [{"Content-Type",
                                                   "text/javascript"} | 
                                                  Headers],
                                                 Data)
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @spec do_post(Req, Handlers, Session) -> ok.
%% 
%% @doc 
%%  Handle incoming post requests.
%% @end
%%--------------------------------------------------------------------
do_post(Req = #req{uri=Uri}, _Handlers, Session) ->
    RFile = filename:split(Uri),
    case get_session_id(Req) of
        undefined ->
            {Id, SessionCookie} = setup_session_cookie(Req, Session),
            carre_client:respond(Req, "200 ok", [{"Content-Type",
                                                  "text/javascript"},
                                                 SessionCookie],
                                 handle_post(RFile, Req, Id));
        Id ->
            carre_client:respond(Req, "200 ok", [{"Content-Type",
                                                   "text/javascript"}],
                                                 handle_post(RFile, Req, Id))
    end.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%% Dispatch the request to the handler. if the handler can't handle
%% it (by casting a function_clause problem) then try the next one.
%% @spec dispatch(Handlers, Uri, Req, Id) -> Result.
%% @end
%%--------------------------------------------------------------------
dispatch([Handler | T], Uri, Req, Id) ->
    try 
        Handler:dispatch(Uri, Req, Id) 
    catch
        error:function_clause ->
            dispatch(T, Uri, Req, Id)
    end;
dispatch([], _, _, _) ->
   {[], "ok"}.

%%--------------------------------------------------------------------
%% @spec handle_post(Uri, Req, Id) ->
%% 
%% @doc 
%%  Parse the uri and actualy direct the call to the appropriate tercio
%%  handler.
%% @end
%%--------------------------------------------------------------------
handle_post(["/", "call", Name], #req{body=Body}, Id) ->
    Values = carre_post:parse(Body),
    Timeout = get_value("timeout", Values),
    Message = get_value("message", Values),
    Pid = tercio:worker(),
    case Timeout of
        undefined ->
            tercio:call(Pid, Name, Id, Message);
        Else ->
            tercio:call(Pid, Name, Id, Message, list_to_integer(Else))
    end,
    tercio:close(Pid);
handle_post(["/", "cast", Name], #req{body=Body}, Id) ->
    Values = carre_post:parse(Body),
    Message = get_value("message", Values),
    Pid = tercio:worker(),
    tercio:cast(Pid, Name, Id, Message),
    tercio:close(Pid),
    "ok".


%%--------------------------------------------------------------------
%% @spec scan_for_problems(ListOfDirs) -> problem | ok.
%% 
%% @doc 
%%  Scans the list of directories for possible problems. This most
%% includes attempts to escape the directory structure.
%% @end
%%--------------------------------------------------------------------
scan_for_problems([".." | _]) ->
    problem;
scan_for_problems(["." | _]) ->
    problem;
scan_for_problems([_ | T]) ->
    scan_for_problems(T);
scan_for_problems([]) ->
    ok.


%%--------------------------------------------------------------------
%% @spec get_session_id(Req) -> Id | undefined.
%% 
%% @doc 
%%  Get the session id from the list of cookies.
%% @end
%%--------------------------------------------------------------------
get_session_id(#req{headers=Headers}) ->
    get_session_id(Headers);
get_session_id([{'Cookie', Value} | _]) ->
    parse_out_id(Value, []);
get_session_id([_ | T]) ->
    get_session_id(T);
get_session_id([]) ->
    undefined.

%%--------------------------------------------------------------------
%% @spec parse_out_id(Cookie, NAcc) -> Id.
%% 
%% @doc 
%%  Parse the cookie looking for the session id.
%% @end
%%--------------------------------------------------------------------
parse_out_id([$= | T], NAcc) ->
    case lists:reverse(NAcc) of
        "SESSION_ID" ->
            {Value, _} = parse_value(T, []),
            Value;
        _Else ->
            {_, Rest} = parse_value(T, []),
            parse_out_id(Rest, [])
    end;
parse_out_id([H | T], NAcc) ->
    parse_out_id(T, [H | NAcc]);
parse_out_id([], _) ->
    undefined.

%%--------------------------------------------------------------------
%% @spec parse_value(Cookie, Acc) -> Value
%% 
%% @doc 
%%  Parse a value from the name value pairs of a cookie.
%% @end
%%--------------------------------------------------------------------
parse_value([$; | T], Acc) ->
    {lists:reverse(Acc), T};
parse_value([H | T], Acc) ->
    parse_value(T, [ H | Acc]);
parse_value([], Acc) ->
    {lists:reverse(Acc), []}.

%%--------------------------------------------------------------------
%% @spec setup_session_cookie(SessionType, Req) -> {Id, CookieHeader}.
%% 
%% @doc 
%%  Creates a 'uniqish' id and returns it along with a preformatted 
%%  cookie.
%% @end
%%--------------------------------------------------------------------
setup_session_cookie(#req{sock=Sock}, "session") ->
    Id = case inet:peername(Sock) of
             {ok, {Address, _}} ->
                 session_id(Address, "session");
             _ ->
                 session_id("noaddress", "session")
         end,
    {Id, {"Set-Cookie", lists:flatten(["SESSION_ID=",
                                       Id, "; VERSION=1"])}};
setup_session_cookie(#req{sock=Sock}, Session) when 
  is_integer(Session) ->
    EndDate = calendar:gregorian_seconds_to_datetime(
                calendar:datetime_to_gregorian_seconds(
                  calendar:universal_time()) + Session),
    CookieEnd = httpd_util:rfc1123_date(EndDate),
    Id = case inet:peername(Sock) of
             {ok, {Address, _}} ->
                 session_id(Address, "session");
             _ ->
                 session_id("noaddress", "session")
         end,
    {Id, {"Set-Cookie", lists:flatten(["SESSION_ID=",
                                       Id, "; VERSION=1; expires=",
                                       CookieEnd])}}.
    

%%--------------------------------------------------------------------
%% @spec session_id(Address, Session) -> SessionId.
%% 
%% @doc 
%%  Create a session id that has a good chance of being unique.
%% @end
%%--------------------------------------------------------------------
session_id({P1, P2, P3, P4}, Session) ->
    N=node(),
    Random  = random:uniform(1500000000),
    {MS,S,US} = erlang:now(),
    md5([integer_to_list(Random), ".", Session, ".", 
         integer_to_list(P1 + P2 + P3 + P4), ".", 
         atom_to_list(N),  ".", 
         integer_to_list(MS), ".",
         integer_to_list(S), ".", 
         integer_to_list(US)]).

%%--------------------------------------------------------------------
%% @spec md5(Item) -> Md5HexString.
%% 
%% @doc 
%%  Convert teh item passed in to a hexstring md5 value. 
%% @end
%%--------------------------------------------------------------------
md5(Item) ->
    Bin = erlang:md5(lists:flatten(Item)),
        hex(0, Bin, size(Bin), []).

%%--------------------------------------------------------------------
%% @spec hex(Count, Binary, Size, Acc)  -> HexString.
%% 
%% @doc 
%%  Convert a numeric value to a hex string.
%% @end
%%--------------------------------------------------------------------
hex(Count, Binary, Size, Acc) when Count < Size ->
    <<_:Count/binary, H1:4, H2:4, _/binary>> = Binary,
        hex(Count + 1, Binary, Size, [hex(H2), hex(H1) | Acc]);
hex(_Count, _Binary, _Size, Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @spec hex(Number) -> HexChar.
%% 
%% @doc 
%%  Convert a number to a hex character.
%% @end
%%--------------------------------------------------------------------
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F;
hex(I) ->  $0 + I.


%%--------------------------------------------------------------------
%% @spec get_values(Name, Values) -> Value | undefined.
%% 
%% @doc 
%%   Get the value from the list of name value pairs.
%% @end
%%--------------------------------------------------------------------
get_value(Name, [{Name, Value} | _]) ->
    Value;
get_value(Name, [ _ | T ]) ->
    get_value(Name, T);
get_value(_Name, []) ->
    undefined.
