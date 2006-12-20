%%%-------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%% 
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(trc_json).

-compile(export_all).



number([$- | T]) ->
    digit19(T, [$-]);
number(Stream) -> 
    digit(Stream, [], front).




%%--------------------------------------------------------------------
%% @spec digit19(Stream, Acc) -> Acc2.
%% 
%% @doc 
%%  Parse from the stream ensuring that the digits has a length of 
%%  between 1 and 9.
%% @end
%%--------------------------------------------------------------------
digit19([$1 | T], Acc) ->
    digit(T, [$1 | Acc], front);
digit19([$2 | T], Acc) ->
    digit(T, [$2 | Acc], front);
digit19([$3 | T], Acc) ->
    digit(T, [$3 | Acc], front);
digit19([$4 | T], Acc) ->
    digit(T, [$4 | Acc], front);
digit19([$5 | T], Acc) ->
    digit(T, [$5 | Acc], front);
digit19([$6 | T], Acc) ->
    digit(T, [$6 | Acc], front);
digit19([$7 | T], Acc) ->
    digit(T, [$7 | Acc], front);
digit19([$8 | T], Acc) ->
    digit(T, [$8 | Acc], front);
digit19([$9 | T], Acc) ->
    digit(T, [$9 | Acc], front);
digit19(Else, Acc) ->
    decimal(Else, Acc).

digit([$0 | T], Acc, Next) ->
    digit(T, [$0 | Acc], Next);
digit([$1 | T], Acc, Next) ->
    digit(T, [$1 | Acc], Next);
digit([$2 | T], Acc, Next) ->
    digit(T, [$2 | Acc], Next);
digit([$3 | T], Acc, Next) ->
    digit(T, [$3 | Acc], Next);
digit([$4 | T], Acc, Next) ->
    digit(T, [$4 | Acc], Next);
digit([$5 | T], Acc, Next) ->
    digit(T, [$5 | Acc], Next);
digit([$6 | T], Acc, Next) ->
    digit(T, [$6 | Acc], Next);
digit([$7 | T], Acc, Next) ->
    digit(T, [$7 | Acc], Next);
digit([$8 | T], Acc, Next) ->
    digit(T, [$8 | Acc], Next);
digit([$9 | T], Acc, Next) ->
    digit(T, [$9 | Acc], Next);
digit(Stream, Acc, Next) ->
    digit_next(Stream, Acc, Next).

decimal([$.| T], Acc) when length(T) > 0 ->
    digit(T, [$. | Acc], decimal);
decimal(Stream, Acc) ->
    integer_end(Stream, Acc).

exponent([$e, $+ | T], Acc) ->
    digit(T, [$+, $e | Acc], exponent);
exponent([$E, $+ | T], Acc) ->
    digit(T, [$+, $E | Acc], exponent);
exponent([$e, $- | T], Acc) ->
    digit(T, [$-, $e | Acc], exponent);
exponent([$E, $- | T], Acc) ->
    digit(T, [$-, $E | Acc], exponent);
exponent(Stream, Acc) ->
    float_end(Stream, Acc).

integer_end([H | T], Acc) 
  when H == $\s; H == $\t;
       H == $\n; H == $\l ->
    {list_to_integer(lists:reverse(Acc)), T};
integer_end([], Acc) ->
    {list_to_integer(lists:reverse(Acc)), []}.


float_end([H | T], Acc) 
  when H == $\s; H == $\t;
       H == $\n; H == $\l ->
    {list_to_float(lists:reverse(Acc)), T};
float_end([], Acc) ->
    {list_to_float(lists:reverse(Acc)), []}.
digit_next(Stream, Acc, front) ->
    decimal(Stream, Acc);
digit_next(Stream, Acc, decimal) ->
    exponent(Stream, Acc);
digit_next(Stream, Acc, exponent) ->
    float_end(Stream, Acc).


