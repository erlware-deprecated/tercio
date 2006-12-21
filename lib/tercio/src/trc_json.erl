%%%-------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%% 
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(trc_json).

-include("eunit.hrl").

-compile(export_all).

-export([decode/1, encode/1]).

-define(LOC_1, 1).
-define(LOC_2, 16).
-define(LOC_3, 256).
-define(LOC_4, 4096).

%%--------------------------------------------------------------------
%% @spec decode(Stream) -> {ParsedJson, UnparsedRemainder}
%% 
%% @doc 
%%  Parses the incoming stream into valid json objects. 
%%  ``
%%   JSON   ==   Erlang
%%   Array       List
%%   String      List
%%   Number      Number
%%   Object      PropList
%%   Ident       String
%%  ''
%%  This decode function parses a superset of json, in that single, un
%%  quoted words are parsed into strings. This makes it easer to 
%%  use json as a config language.
%% @end
%%--------------------------------------------------------------------
decode(Stream) ->
    value(Stream).


%%--------------------------------------------------------------------
%% @spec  encode(DataObjects) -> List.
%% 
%% @doc 
%%  Parses a list of data objects into a list. The list is a deeply 
%%  nested list and should be flattened if you wish to use it as a 
%%  string. Otherwise, io functions will flatten the list for you.
%%  ``
%%   Erlang    ==     JSON
%%   {string, Val}    String
%%   List             Array
%%   Atom             String
%%   PropList         Object
%%   Number           Number
%%  ''
%% @end
%%--------------------------------------------------------------------
encode(Data = [{string, _} | _]) ->
    lists:reverse(encode_array(Data, []));
encode(Data = [{_, _} | _]) ->
    encode_object(Data, []);
encode(Data) when is_list(Data) ->
    lists:reverse(encode_array(Data, []));
encode({string, Data}) ->
    encode_string(Data);
encode(Data) when is_integer(Data) ->
    encode_integer(Data);
encode(Data) when is_float(Data) ->
    encode_float(Data);
encode(true) ->
    atom_to_list(true);
encode(false) ->
    atom_to_list(false);
encode(null) ->
    atom_to_list(null);
encode(Data) when is_atom(Data)->
    encode_string(Data).


%%=============================================================================
%% Internal Functions
%%=============================================================================
encode_string({string, Value}) ->
    [$\", Value, $\"];
encode_string(Value) when is_atom(Value) ->
    [$\", atom_to_list(Value), $\"];
encode_string(Value) when is_list(Value) ->
    [$\", Value, $\"].

encode_integer(Value) ->
    integer_to_list(Value).

encode_float(Value) ->
    float_to_list(Value).


encode_array([H | T], []) ->
    encode_array(T, [encode(H), $[]);
encode_array([H | T], TAcc) ->
    encode_array(T, [encode(H), $, | TAcc]);
encode_array([], []) ->
    [$], $[];
encode_array([], TAcc) ->
    [$] | TAcc].

encode_object([{Key, Value} | T], []) ->
    encode_object(T, [encode_string(Key), $:, 
                      encode(Value), $}]);
encode_object([{Key, Value} | T], TAcc) ->
    encode_object(T, [encode_string(Key), $:, 
                      encode(Value), $, | TAcc]);
encode_object([], []) ->
    [${, $}];
encode_object([], TAcc) ->
    [${ | TAcc].


value([$\" | T]) ->
    string_body(T, []);
value([$- | T]) ->
    digit19(T, [$-]);
value([$0 | T]) ->
    digit(T, [$0], front); 
value([$1 | T]) ->
    digit(T, [$1], front);
value([$2 | T]) ->
    digit(T, [$2], front);
value([$3 | T]) ->
    digit(T, [$3], front);
value([$4 | T]) ->
    digit(T, [$4], front);
value([$5 | T]) ->
    digit(T, [$5], front);
value([$6 | T]) ->
    digit(T, [$6], front);
value([$7 | T]) ->
    digit(T, [$7], front);
value([$8 | T]) ->
    digit(T, [$8], front);
value([$9 | T]) ->
    digit(T, [$9], front);
value([$[ | T]) ->
    array_body(T, []);
value([${ | T]) ->
    object_body(T, []);
value([$t, $r, $u, $e | T]) ->
    {true, T};
value([$f, $a, $l, $s, $e | T]) ->
    {false, T};
value([$n, $u, $l, $l | T]) ->
    {null, T};
value([$\s | T]) ->
    value(T);
value([$\t | T]) ->
    value(T);
value([$\r | T]) ->
    value(T);
value([$\n | T]) ->
    value(T);
value(Stream) ->
    ident(Stream, []).


array_body([$] | T], Acc) ->
    {lists:reverse(Acc), T};
array_body([$, | T], Acc) ->
    array_body(T, Acc);
array_body([$\s | T], Acc) ->
    array_body(T, Acc);
array_body([$\t | T], Acc) ->
    array_body(T, Acc);
array_body([$\n | T], Acc) ->
    array_body(T, Acc);
array_body([$\r | T], Acc) ->
    array_body(T, Acc);
array_body(Stream, Acc) ->
    {Value, Rest} = value(Stream),
    array_body(Rest, [Value | Acc]).

object_body([$} | T], Acc) ->
    {Acc, T};
object_body([$, | T], Acc) ->
    object_body(T, Acc);
object_body([$\s | T], Acc) ->
    object_body(T, Acc);
object_body([$\t | T], Acc) ->
    object_body(T, Acc);
object_body([$\r | T], Acc) ->
    object_body(T, Acc);
object_body([$\n | T], Acc) ->
    object_body(T, Acc);
object_body(Else, Acc) ->
    {Key, Rest1} = key(Else),
    Rest2 = find($:, Rest1),
    {Value, Rest3} = value(Rest2),
    object_body(Rest3, [{Key, Value} | Acc]).

find(Delim, [Delim | T]) ->
    T;
find(Delim, [$\s | T]) ->
    find(Delim, T);
find(Delim, [$\t | T]) ->
    find(Delim, T);
find(Delim, [$\r | T]) ->
    find(Delim, T);
find(Delim, [$\n | T]) ->
    find(Delim, T).

key([$\" | T]) ->
    string_body(T, []);
key([$\s | T]) ->
    key(T);
key([$\t | T]) ->
    key(T);
key([$\r | T]) ->
    key(T);
key([$\n | T]) ->
    key(T);
key(Stream) ->
    ident(Stream, []).



ident([H | T], Acc) when H >= $a, H =< $z ->
    ident(T, [H | Acc]);
ident([H | T], Acc) when H >= $A, H =< $Z ->
    ident(T, [H | Acc]);
ident([$_ | T], Acc) ->
    ident(T, [$_ | Acc]);
ident([H | T], Acc) when H >= $0, H =< $9 ->
    ident(T, [ H | Acc]);
ident([$\s | T], Acc) ->
    {lists:reverse(Acc), T};
ident([$\t | T], Acc) ->
    {lists:reverse(Acc), T};
ident([$\r | T], Acc) ->
    {lists:reverse(Acc), T};
ident([$\n | T], Acc) ->
    {lists:reverse(Acc), T};
ident([], Acc) ->
    {lists:reverse(Acc), []};
ident(Else, Acc) when length(Acc) > 0->
    {lists:reverse(Acc), Else}.

string_body([$\\, $\" | T], Acc) ->
    string_body(T, [$\" | Acc]);
string_body([$\\, $/ | T], Acc) ->
    string_body(T, [$/ | Acc]);
string_body([$\\, $\\ | T], Acc) ->
    string_body(T, [$\\ | Acc]);
string_body([$\\, $b | T], Acc) ->
    string_body(T, [$\b | Acc]);
string_body([$\\, $f | T], Acc) ->
    string_body(T, [$\f | Acc]);
string_body([$\\, $n | T], Acc) ->
    string_body(T, [$\n | Acc]);
string_body([$\\, $r | T], Acc) ->
    string_body(T, [$\r | Acc]);
string_body([$\\, $t | T], Acc) ->
    string_body(T, [$\t | Acc]);
string_body([$\\, $u | T], Acc) ->
    parse_hex_digit(T, Acc, []);
string_body([$\" | T], Acc) ->
    {lists:reverse(Acc), T};
string_body([H | T], Acc) ->
    string_body(T, [H | Acc]).


parse_hex_digit([$0 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$0 | HexAcc]);
parse_hex_digit([$1 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$1 | HexAcc]);
parse_hex_digit([$2 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$2 | HexAcc]);
parse_hex_digit([$3 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$3 | HexAcc]);
parse_hex_digit([$4 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$4 | HexAcc]);
parse_hex_digit([$5 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$5 | HexAcc]);
parse_hex_digit([$6 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$6 | HexAcc]);
parse_hex_digit([$7 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$7 | HexAcc]);
parse_hex_digit([$8 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$8 | HexAcc]);
parse_hex_digit([$9 | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$9 | HexAcc]);
parse_hex_digit([$A | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$A | HexAcc]);
parse_hex_digit([$a | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$A | HexAcc]);
parse_hex_digit([$B | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$B | HexAcc]);
parse_hex_digit([$b | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$B | HexAcc]);
parse_hex_digit([$C | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$C | HexAcc]);
parse_hex_digit([$c | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$C | HexAcc]);
parse_hex_digit([$D | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$D | HexAcc]);
parse_hex_digit([$d | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$D | HexAcc]);
parse_hex_digit([$E | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$E | HexAcc]);
parse_hex_digit([$e | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$E | HexAcc]);
parse_hex_digit([$F | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$F | HexAcc]);
parse_hex_digit([$f | T], Acc, HexAcc) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$F | HexAcc]);
parse_hex_digit(Stream, Acc, HexAcc) when length(HexAcc) == 4 ->
    [D1, D2, D3, D4] = HexAcc,
    Char = ((c2n(D1) * ?LOC_1) + 
            (c2n(D2) * ?LOC_2) +
            (c2n(D3) * ?LOC_3) +
            (c2n(D4) * ?LOC_4)),
    string_body(Stream, [Char | Acc]).


c2n(Char) when Char < 58 ->
    Char - 48;
c2n(Char) ->
    Char - 54.

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

%%--------------------------------------------------------------------
%% @spec digit(Stream, Acc, Next) -> {Res, Rest}.
%% 
%% @doc 
%%  Parse out the specified digit set.
%% @end
%%--------------------------------------------------------------------
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
exponent([$E | T], Acc) ->
    digit(T, [$E | Acc], exponent);
exponent([$e | T], Acc) ->
    digit(T, [$e | Acc], exponent);
exponent(Stream, Acc) ->
    float_end(Stream, Acc).

integer_end(Stream, Acc) ->
    {list_to_integer(lists:reverse(Acc)), Stream}.


float_end(Stream, Acc) ->
    {list_to_float(lists:reverse(Acc)), Stream}.



digit_next(Stream, Acc, front) ->
    decimal(Stream, Acc);
digit_next(Stream, Acc, decimal) ->
    exponent(Stream, Acc);
digit_next(Stream, Acc, exponent) ->
    float_end(Stream, Acc).

%%=============================================================================
%% Unit tests
%%=============================================================================
encode_string_test() ->
    ?assertMatch("\"Hello\"", lists:flatten(encode({string, "Hello"}))),
    ?assertMatch("\"hello\"", lists:flatten(encode('hello'))).

encode_number_test() ->
    ?assertMatch("430", lists:flatten(encode(430))),
    ?assertMatch("4303432", lists:flatten(encode(4303432))),
    ?assertMatch("4.30000000000000000000e+01", lists:flatten(encode(43.00))),
    ?assertMatch("3.22232219999999983884e+02", 
                 lists:flatten(encode(322.23222))).

encode_array_test() ->
    ?assertMatch("[33,43,53]", lists:flatten(encode([33, 43, 53]))),
    ?assertMatch("[\"String\",34,\"song\"]", lists:flatten(encode([{string, 
                                                                    "String"},
                                                                   34, 
                                                                   song]))),
    ?assertMatch("[{\"Goodbye\":true,\"Hello\":44},43,54]",
                 lists:flatten(encode([[{{string, "Hello"}, 44},
                                        {{string, "Goodbye"}, true}],
                                       43, 54]))).

encode_object_test() ->
    ?assertMatch("{\"Hello\":\"Hel\",\"Super\":421}",
                 lists:flatten(encode([{{string, "Super"}, 421},
                                {'Hello','Hel'}]))).



number_test() ->
    ?assertMatch({44, []}, value("44")),
    ?assertMatch({-44, []}, value("-44")),
    ?assertMatch({44.00, []}, value("44.00")),
    ?assertMatch({-44.01, []}, value("-44.01")),
    ?assertMatch({44.00e+33, []}, value("44.00e+33")),
    ?assertMatch({44.00e33, []}, value("44.00e33")),
    ?assertMatch({44.00e-10, []}, value("44.00e-10")),
    ?assertMatch({42.44, []}, value("42.44")),
    ?assertMatch({41.33, []}, value("41.33")),
    ?assertMatch({0, []}, value("0")).


string_test() ->
    ?assertMatch({"Hello World", []},
                 value("\"Hello World\"")),
    ?assertMatch({"Hello\n World", []},
                 value("\"Hello\n World\"")),
    ?assertMatch({"Hello\" World", []},
                 value("\"Hello\\\" World\"")),
    ?assertMatch({"Hello\\ World", []},
                 value("\"Hello\\ World\"")),
    ?assertMatch({"Hello\/ World", []},
                 value("\"Hello\/ World\"")),
    ?assertMatch({"Hello\b World", []},
                 value("\"Hello\b World\"")),
    ?assertMatch({"Hello\f World", []},
                 value("\"Hello\f World\"")),
    ?assertMatch({"Hello\n World", []},
                 value("\"Hello\n World\"")),
    ?assertMatch({"Hello\r World", []},
                 value("\"Hello\r World\"")),
    ?assertMatch({"Hello\t World", []},
                 value("\"Hello\t World\"")),
    ?assertMatch({"Hello% World", []},
                 value("\"Hello\\u0025 World\"")).

boolean_test() ->
    ?assertMatch({true, []}, value("true")),
    ?assertMatch({false, []}, value("false")).

null_test() ->
    ?assertMatch({null, []}, value("null")).

ident_test() ->
    ?assertMatch({"Hello", []}, value("Hello")),
    ?assertMatch({"boo88", []}, value("boo88")),
    ?assertMatch({"bock", [$:]}, value("bock:")),
    ?assertMatch({"bock", [${]}, value("bock{")),
    ?assertMatch({"bock", [$[]}, value("bock[")).


glossary_test() ->
    ?assertMatch({[{"glossary",
                   [{"GlossDiv",
                     [{"GlossList",
                       [{"GlossEntry",
                         [{"GlossSee","markup"},
                          {"GlossDef",
                           [{"GlossSeeAlso",["GML","XML"]},
                            {"para","A meta-mars DocBook."}]},
                          {"Abbrev","ISO 8879:1986"},
                          {"Acronym","SGML"},
                          {"GlossTerm","Standareralized"},
                          {"SortAs","SGML"},
                          {"ID","SGML"}]}]},
                      {"title","S"}]},
                    {"title","example glossary"}]}],
                  []},  
                 value("{ "   
                      "\"glossary\": { "
                      "  \"title\": \"example glossary\","
                      " \"GlossDiv\": {"
                      "      \"title\": \"S\", "
                      "  \"GlossList\": { "
                      "          \"GlossEntry\": {"
                      "              \"ID\": \"SGML\","
                      "            \"SortAs\": \"SGML\","
                      "            \"GlossTerm\": \"Standareralized\", "
                      "            \"Acronym\": \"SGML\", "
                      "            \"Abbrev\": \"ISO 8879:1986\","
                      "            \"GlossDef\": { "
                      "                  \"para\": \"A meta-mars DocBook.\", "
                      "             \"GlossSeeAlso\": [\"GML\", \"XML\"]"
                      "             },"
                      "            \"GlossSee\": \"markup\" "
                      "          }"
                      "      }"
                      "  }"
                      "}}")).

menu_test() ->
    ?assertMatch({[{"menu",
                    [{"popup",
                      [{"menuitem",
                        [[{"onclick","CreateNewDoc()"},
                          {"value","New"}],
                         [{"onclick","OpenDoc()"},
                          {"value","Open"}],
                         [{"onclick","CloseDoc()"},
                          {"value","Close"}]]}]},
                     {"value","File"},
                     {"id","file"}]}],
                  []},
      value("{\"menu\": {"
            "  \"id\": \"file\","
            "  \"value\": \"File\","
            "  \"popup\": {"
            "      \"menuitem\": ["
            "          {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, "
            "          {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},"
            "          {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} "
            "                    ]"
            "   }"
            "}}")).

widget_test() ->
    ?assertMatch({[{"widget",
                    [{"text",
                      [{"onMouseUp","sun1.opacity = (sun1.opacity / 100) * 90;"},
                       {"alignment","center"},
                       {"vOffset",100},
                       {"hOffset",250},
                       {"name","text1"},
                       {"style","bold"},
                       {"size",36},
      {"data","Click Here"}]},
                     {"image",
                      [{"alignment","center"},
                       {"vOffset",250},
                       {"hOffset",250},
                       {"name","sun1"},
                       {"src","Images/Sun.png"}]},
                     {"window",
                      [{"height",500},
                       {"width",500},
                       {"name","main_window"},
                       {"title","Sample Konfabulator Widget"}]},
                     {"debug","on"}]}],
                  []},
                 
                 value("{\"widget\": {"
                       "    debug: on,"
                       "    window: {"
                       "        title: \"Sample Konfabulator Widget\","
                       "        name: \"main_window\","
                       "        \"width\": 500,"
                       "        \"height\": 500"
                       "    },"
                       "    \"image\": { "
                       "        \"src\": \"Images/Sun.png\","
                       "        \"name\": \"sun1\","
                       "        \"hOffset\": 250,"
                       "        \"vOffset\": 250,"
                       "        \"alignment\": \"center\""
                       "    },"
                       "    \"text\": {"
                       "        \"data\": \"Click Here\","
                       "        \"size\": 36,"
                       "        \"style\": \"bold\","
                       "        \"name\": \"text1\","
                       "        \"hOffset\": 250,"
                       "        \"vOffset\": 100,"
                       "        \"alignment\": \"center\","
                       "        \"onMouseUp\": \"sun1.opacity = (sun1.opacity / 100) * 90;\""
                       "    }"
                       "}}")).  

