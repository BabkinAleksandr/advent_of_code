-module(p1).
-export([main/0]).
-include_lib("stdlib/include/assert.hrl").
-define(CORRECT_ANSWER, 54697).

main() ->
    Sum = foldl_recursive(fun processor/3, 0, fun get_next_line/0),
    ?assertEqual(Sum, ?CORRECT_ANSWER),
    io:format("~n/2023/d1/P1 result: ~p~n", [Sum]).

-spec get_next_line() -> string() | finished.
get_next_line() ->
    case io:get_line(standard_io, "") of
        Line when is_list(Line) -> Line;
        _ -> finished
    end.

-spec foldl_recursive(Fun, Acc0, NextElem) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn, Index :: R) -> AccOut),
      Acc0 :: R,
      Acc1 :: R,
      AccIn :: R,
      AccOut :: R,
      NextElem :: fun(() -> T | finished),
      T :: string(),
      R :: integer().
foldl_recursive(Fun, Acc, NextElem) when is_function(Fun, 3) and is_function(NextElem, 0) ->
    foldl_recursive(Fun, Acc, NextElem, 0);
foldl_recursive(_, _, _) ->
    throw(bad_arguments).

foldl_recursive(Fun, Acc, NextElem, I) ->
    case NextElem() of
        finished -> Acc;
        Item -> foldl_recursive(Fun, Fun(Item, Acc, I), NextElem, I + 1)
    end.

-spec processor(Line, Acc, Index) -> Result when
      Line :: string(),
      Acc :: integer(),
      Index :: integer(),
      Result :: integer().
processor(Line, Acc, Index) ->
    case parse_line(Line) of
        {number, N} ->
            Acc + N;
        {error, _} ->
            io:format("Failed to parse ~p~n", [Index]),
            throw(error)
    end.

-spec parse_line(Line) -> Result when
      Line :: string(),
      Result :: {error, string()} | {number, integer()}.
parse_line(Line) ->
    case parse_line(Line, {not_set, not_set}) of
        {not_set, not_set} ->
            {error, "Failed to parse line"};
        {First, Last} when is_number(First) and is_number(Last) ->
            {number, (First * 10 + Last)}
    end.

-spec parse_line(String, Numbers) -> Numbers when
      String :: string(),
      Numbers :: {not_set, not_set} | {integer(), integer()}.
parse_line([], Numbers) ->
    Numbers;
parse_line([C|Rest], Numbers) when C >= $0 andalso C =< $9 ->
    I = C - $0,
    case Numbers of
        {not_set, not_set} ->
            parse_line(Rest, {I, I});
        {FirstDigit, _} ->
            parse_line(Rest, {FirstDigit, I})
    end;
parse_line([_|Rest], Numbers) ->
    parse_line(Rest, Numbers).

