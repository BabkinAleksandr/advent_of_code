-module(p1).
-export([main/0]).
-include_lib("stdlib/include/assert.hrl").
-define(CORRECT_ANSWER, 1603498).

main() ->
    % Read input to two lists
    {ListL, ListR} = foldl_recursive(fun processor/3, {[], []}, fun get_next_line/0),
    Sum = lists:foldl(
        fun({L, R}, Acc) -> Acc + abs(L - R) end,
        0,
        % sort lists and zip them (make pairs of corresponding elements)
        lists:zip(lists:sort(ListL), lists:sort(ListR))
    ),
    ?assertEqual(Sum, ?CORRECT_ANSWER),
    io:format("~n/2024/d1/P1 result: ~p~n", [Sum]).

-spec get_next_line() -> string() | finished.
get_next_line() ->
    case io:get_line(standard_io, "") of
        Line when is_list(Line) -> Line;
        _ -> finished
    end.

-spec foldl_recursive(Fun, Acc0, NextElem) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn, Index :: I) -> AccOut),
      Acc0 :: R,
      Acc1 :: R,
      AccIn :: R,
      AccOut :: R,
      NextElem :: fun(() -> T | finished),
      T :: string(),
      I :: integer(),
      R :: {[integer()], [integer()]}.
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
      Acc :: {[integer()], [integer()]},
      Index :: integer(),
      Result :: {[integer()], [integer()]}.
processor(Line, {ListL, ListR}, _Index) ->
    [LStr, RStr] = string:split(Line, " "),
    {L, R} = maybe
        {IntL, _} ?= string:to_integer(string:trim(LStr)),
        {IntR, _} ?= string:to_integer(string:trim(RStr)),
        true = is_integer(IntL),
        true = is_integer(IntR),
        {IntL, IntR}
    else
        _Item -> 
           io:format("Error during parsing: ~p~n", _Item),
           throw(badarg)
    end,
    { [L|ListL], [R|ListR] }.

