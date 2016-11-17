%% Erlang json lib
-module(ejson).

-export([parse_json/1]).

-import(lists, [reverse/1]).

-type json_value_type() :: null | atom | number | boolean | string | array | object.
-type json_parse_state() :: new_start | parse_key | parse_value | parse_end | error.

%% -record(parse_state, {state::json_parse_state(), jsonstring::list()}).
-record(json_item, {key::atom(), value, type::json_value_type()}).

%% export parse_json/1
-spec parse_json(Json) -> {ok, Return, LeftJson} | {error, Reason} when
    Json::list(),
    Return::[Object],
    LeftJson::list(),
    is_record(Object, json_item),
    Reason::list().
parse_json(Json = [${ | _]) ->
    parse_json(remove_blank_element(Json), [], [], [], new_start);
parse_json({error, Reason}) ->
    throw({error, Reason}).

%% internal parse_json/5
-spec parse_json(Json, Key, Value, Result, State) -> {ok, Result} | {error, Reason} when
    Json :: list(),
    Key :: string(),
    Value :: list(),
    Result :: [Object],
    is_record(Object, json_item),
    State :: json_parse_state(),
    Reason :: string().

%% parse_json end here
parse_json([], _, _, Result, _) ->
    {ok, reverse(Result), []};
%% parse object end here
parse_json([$, |Json], [], [], Result, parse_end) ->
    {ok, reverse(Result), Json};
%% parse object end here, and that's last object
parse_json([$}], [], [], Result, _) ->
    {ok, reverse(Result), []};
%% null object
parse_json([$} | Json], [], [], [], parse_key) ->
    {ok, [#json_item{key=null, value=null, type=null}], Json};
parse_json([$} | _], [], [], Result, parse_key) ->
    {ok, reverse(Result), []};
%% next item parse
parse_json([H |Json], [], [], Result, parse_end) ->
    parse_json(Json, [H], [], Result, parse_key);
parse_json([$, |Json], [], [], Result, parse_key) ->
    parse_json(Json, [], [], Result, parse_key);
%% start parse here
parse_json([${ |Json], [], [], Result, new_start) ->
    parse_json(Json, [], [], Result, parse_key);
%% parse object 
parse_json(Json = [${ |_], [$" |RevKey], [], Result, parse_value) ->
    case parse_json(Json, [], [], [], new_start) of
        {error, Reason} -> {error, Reason};
        {ok, HalfResult, LeftJson} -> 
            [$"|Key] = reverse(RevKey),
            Item = #json_item{key = list_to_atom(Key), value = HalfResult, type = object},
            parse_json(LeftJson, [], [], [Item | Result], parse_key)
    end;
%% parse value start
parse_json([$: |Json], Key, [], Result, parse_key) ->
    parse_json(Json, Key, [], Result, parse_value);
%% parse value end
parse_json([H |Json], [$"|TempKey], Value, Result, parse_value) when H == $, orelse H == $} ->
    [$"|Key] = reverse(TempKey),
    JsonKey = list_to_atom(Key),
    try json_type(reverse(Value)) of
        {ok, JsonValue, JsonType} ->
            Object = #json_item{key = JsonKey, value = JsonValue, type = JsonType},
            parse_json(Json, [], [], [Object | Result], parse_end);
        {error, Reason} ->
            parse_json({error, Reason})
    catch
        _:Reason ->
            parse_json({error, Reason})
    end;
%% normal parse key
parse_json([H |Json], Key, Value, Result, parse_key) ->
    parse_json(Json, [H|Key], Value, Result, parse_key);
%% normal parse value
parse_json([H |Json], Key, Value, Result, parse_value) ->
    parse_json(Json, Key, [H|Value], Result, parse_value).


%% return json_type
json_type([]) ->
    throw(error_empty_value);
json_type([$"]) ->
    throw(error_param);
json_type([$" |T]) ->
    [$" | S] = reverse(T),
    {ok, reverse(S), string};
json_type(V = [H | _]) when H >= $0 andalso H =< $9 ->
    try list_to_number(V) of
        Value -> {ok, number, Value}
    catch
        _:Reason -> throw(Reason)
    end;
json_type([$t, $r, $u, $e]) ->
    {ok, true, boolean};
json_type([$f, $a, $l, $s, $e]) ->
    {ok, false, boolean};
json_type([$n, $u, $l, $l]) ->
    {ok, true, boolean};
json_type(A) ->
    {ok, list_to_atom(A), atom}.

%% It's wired that Erlang donot have list_to_number
list_to_number(V) ->
    try list_to_integer(V) of
        Value -> Value
    catch
        _:_ -> list_to_float(V)
    end.

%% remove \t \n \b etc.
remove_blank_element(List) when is_list(List) ->
    remove_blank_element(List, []).

%% -spec remove_blank_element(OrigList::list(), NewList::list()) -> Return :: list()
remove_blank_element([], NewList) ->
    lists:reverse(NewList);
remove_blank_element([H|T], NewList) ->
    case H of
        $  -> remove_blank_element(T, NewList);
        $\n -> remove_blank_element(T, NewList);
        $\r -> remove_blank_element(T, NewList);
        $\t -> remove_blank_element(T, NewList);
        K -> remove_blank_element(T, [K|NewList])
    end.
