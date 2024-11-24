-module(p2).
-export([main/0]).
-include_lib("stdlib/include/assert.hrl").
-define(CORRECT_ANSWER, 72422).

-record(cubes, {
    red = 0 :: integer(),
    green = 0 :: integer(),
    blue = 0 :: integer()
}).

main() ->
    Sum = foldl_recursive(fun processor/2, 0, fun get_next_line/0),
    ?assertEqual(Sum, ?CORRECT_ANSWER),
    io:format("~n/2023/d2/P2 result: ~p~n", [Sum]).

-spec get_next_line() -> string() | finished.
get_next_line() ->
    case io:get_line(standard_io, "") of
        Line when is_list(Line) -> Line;
        _ -> finished
    end.

-spec foldl_recursive(Fun, Acc0, NextElem) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: R,
      Acc1 :: R,
      AccIn :: R,
      AccOut :: R,
      NextElem :: fun(() -> T | finished),
      T :: string(),
      R :: integer().
foldl_recursive(Fun, Acc, NextElem) when is_function(Fun, 2) and is_function(NextElem, 0) ->
    case NextElem() of
        finished -> Acc;
        Item -> foldl_recursive(Fun, Fun(Item, Acc), NextElem)
    end;
foldl_recursive(_, _, _) ->
    throw(badarg).

-spec processor(Line, Acc) -> Result when
      Line :: string(),
      Acc :: integer(),
      Result :: integer().
processor(Line, Acc) ->
    case parse_line(Line) of
        {number, N} ->
            Acc + N;
        no_result ->
            Acc
    end.

-spec parse_line(Line) -> Result when
      Line :: string() | eof,
      Result :: {number, integer()} | no_result.
parse_line(eof) ->
    no_result;
parse_line(GameLine) ->
    [_, TurnsStr] = string:split(GameLine, ":"),
    Cubes = get_cubes_required(string:trim(TurnsStr)),
    {number, Cubes#cubes.red * Cubes#cubes.green * Cubes#cubes.blue}.


-spec get_cubes_required(TurnsStr :: string()) -> #cubes{}.
get_cubes_required(TurnsStr) ->
    lists:foldl(fun get_cubes_required_for_turn/2, #cubes{}, string:split(TurnsStr, ";", all)).

-spec get_cubes_required_for_turn(Turn :: string(), Acc :: #cubes{}) -> #cubes{}.
get_cubes_required_for_turn(Turn, Acc) ->
    Cubes = string:split(Turn, ",", all),
    lists:foldl(fun (T, A) -> merge_cubes(parse_pair(T), A) end, Acc, Cubes).

-spec merge_cubes(C1 :: #cubes{}, C2 :: #cubes{}) -> #cubes{}.
merge_cubes(C1, C2) ->
    #cubes{
       red = lists:max([C1#cubes.red, C2#cubes.red]),
       green = lists:max([C1#cubes.green, C2#cubes.green]),
       blue = lists:max([C1#cubes.blue, C2#cubes.blue])
    }.

-spec parse_pair(Pair :: string()) -> #cubes{}.
parse_pair(Pair) ->
    [Count, Name] = string:split(string:trim(Pair), " "),
    Int = case string:to_integer(Count) of
        {error, _} ->
            io:format("Failed to parse count for: ~p~n", [Count]),
            throw(error);
        {I, _} ->
            I
    end,
    case Name of
        "red" ->
            #cubes{ red = Int };
        "green" ->
            #cubes{ green = Int };
        "blue" ->
            #cubes{ blue = Int };
        Str ->
            io:format("I haven't matched anything! ~s~n", [Str]),
            throw(error)
    end.

