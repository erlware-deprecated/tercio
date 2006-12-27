%%%-------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc 
%%%   Handles the incoming parts of the system.  Parsing and forwarding 
%%%   on to the correct tercio services.
%%% @end
%%% @copyright (C) 2006
%%% Created : 26 Dec 2006 
%%%-------------------------------------------------------------------
-module(carre_handler).

-include("carre.hrl").

%% API
-export([do_get/2, do_post/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec do_get(Req, ServerRoot) ->
%% @doc
%%  Do the get requests. These are by definition requests for files.
%% @end 
%%--------------------------------------------------------------------
do_get(Req = #req{uri=Uri}, ServerRoot) ->
    RFile = filename:split(Uri),
    case scan_for_problems(RFile) of
        problems ->
            carre_client:respond(Req, "403 Forbidden", "");
        ok ->
            case RFile of
                ["/" | Rest] ->
                    AFile = filename:join([ServerRoot | Rest]),
                    process_file(Req, AFile);
                _Other ->
                    AFile = filename:join([ServerRoot | RFile]),
                    process_file(Req, AFile)
            end
    end.

do_post(#req{}, _ServerRoot) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
process_file(Req, File) ->
    case string:substr(filename:extension(File), 2) of
        "html" ->
            process_file(Req, "text/html", File);
        "htm" ->
            process_file(Req, "text/html", File);
        "xhtml"->
            process_file(Req, "text/html", File);
        "js" ->
            process_file(Req, "text/javascript", File);
        "gif" ->
            process_file(Req, "image/gif", File);
        "shtml" ->
            process_file(Req, "text/html", File);
        "avi" ->
            process_file(Req, "video/avi", File);
        "bin" ->
            process_file(Req, "application/octet-stream", File);
        "bmp" ->
            process_file(Req, "image/bmp", File);
        "doc" ->
            process_file(Req, "application/msword", File);
        "dvi" ->
            process_file(Req, "application/x-dvi", File);
        "gz" ->
            process_file(Req, "application/x-gzip", File);
        "gzip" ->
            process_file(Req, "application/x-gzip", File);
        "jpeg" ->
            process_file(Req, "image/jpeg", File);
        "jpg" ->
            process_file(Req, "image/jpeg", File);
        "mp3" ->
            process_file(Req, "audio/mpeg3", File);
        "mpeg" ->
            process_file(Req, "video/mpeg", File);
        "pdf" ->
            process_file(Req, "application/pdf", File);
        "png" ->
            process_file(Req, "image/png", File);
        "pps" ->
            process_file(Req, "application/mspowerpoint", File);
        "ps" ->
            process_file(Req, "application/postscript", File);
        "sgml" ->
            process_file(Req, "text/sgml", File);
        "text" ->
            process_file(Req, "text/plain", File);
        "tgz" ->
            process_file(Req, "application/gnutar", File);
        "tif" ->
            process_file(Req, "image/tiff", File);
        "tiff" ->
            process_file(Req, "image/tiff", File);
        "txt" ->
            process_file(Req, "text/plain", File);
        "xml" ->
            process_file(Req, "text/xml", File);
        "zip" ->
            process_file(Req, "application/zip", File);
        _ ->
            process_file(Req, "text/plain", File)
    end.

process_file(Req, Mime, File) ->
    case file:read_file(File) of
        {ok, Data} ->
            carre_client:respond(Req, "200 ok",
                                 [{"Content-Type",
                                   Mime}],
                                 Data);
        {error, _} ->
            carre_client:respond(Req, "404 Not found",
                                 "")
    end.

scan_for_problems([".." | _]) ->
    problem;
scan_for_problems(["." | _]) ->
    problem;
scan_for_problems([_ | T]) ->
    scan_for_problems(T);
scan_for_problems([]) ->
    ok.
