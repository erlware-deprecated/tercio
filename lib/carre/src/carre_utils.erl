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
%%%   Helps by providing a way to easily output system parts.
%%% @end
%%% @copyright (C) 2006
%%% Created : 26 Dec 2006 
%%%---------------------------------------------------------------------------
-module(carre_utils).


%% API
-export([process_file/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec process_file(File) -> ok.
%% 
%% @doc 
%%  Start processing the file, mostly by figuring out the mime type.
%% @end
%%--------------------------------------------------------------------
process_file(File) ->
    case string:substr(filename:extension(File), 2) of
        "html" ->
            process_file("text/html", File);
        "htm" ->
            process_file("text/html", File);
        "xhtml"->
            process_file("text/html", File);
        "js" ->
            process_file("text/javascript", File);
        "gif" ->
            process_file("image/gif", File);
        "shtml" ->
            process_file("text/html", File);
        "avi" ->
            process_file("video/avi", File);
        "bin" ->
            process_file("application/octet-stream", File);
        "bmp" ->
            process_file("image/bmp", File);
        "doc" ->
            process_file("application/msword", File);
        "dvi" ->
            process_file("application/x-dvi", File);
        "gz" ->
            process_file("application/x-gzip", File);
        "gzip" ->
            process_file("application/x-gzip", File);
        "jpeg" ->
            process_file("image/jpeg", File);
        "jpg" ->
            process_file("image/jpeg", File);
        "mp3" ->
            process_file("audio/mpeg3", File);
        "mpeg" ->
            process_file("video/mpeg", File);
        "pdf" ->
            process_file("application/pdf", File);
        "png" ->
            process_file("image/png", File);
        "pps" ->
            process_file("application/mspowerpoint", File);
        "ps" ->
            process_file("application/postscript", File);
        "sgml" ->
            process_file("text/sgml", File);
        "text" ->
            process_file("text/plain", File);
        "tgz" ->
            process_file("application/gnutar", File);
        "tif" ->
            process_file("image/tiff", File);
        "tiff" ->
            process_file("image/tiff", File);
        "txt" ->
            process_file("text/plain", File);
        "xml" ->
            process_file("text/xml", File);
        "zip" ->
            process_file("application/zip", File);
        _ ->
            process_file("text/plain", File)
    end.

%%--------------------------------------------------------------------
%% @spec process_file(Mime, File) -> ok.
%% 
%% @doc 
%%  Process the specified file returning its contents with the correct
%%  mime type or an appropriate error.
%% @end
%%--------------------------------------------------------------------
process_file(Mime, File) ->
    case file:read_file(File) of
        {ok, Data} ->
            {[{"Content-Type", Mime}], Data};
        {error, _} ->
            {"404 Not found", [], ""}
    end.

