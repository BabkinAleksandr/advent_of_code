-module(p2).
-export([main/0]).
-include_lib("stdlib/include/assert.hrl").
-define(CORRECT_ANSWER, 25574739).

main() ->
    % Read input to two lists
    {ListL, ListR} = foldl_recursive(fun processor/3, {[], []}, fun get_next_line/0),
    % create a map of occurences of ListR
    RM = lists:foldl(fun collect_occurence/2, #{}, ListR),
    % count occurences
    Sum = lists:foldl(count_similarity(RM), 0, ListL),
    ?assertEqual(Sum, ?CORRECT_ANSWER),
    io:format("~n/2024/d1/P2 result: ~p~n", [Sum]).

-spec collect_occurence(Int, Acc) -> R when
      Int :: integer(),
      Acc :: M,
      R :: M,
      M :: #{K :: integer() => V :: integer()}.
collect_occurence(Int, Acc) ->
    maps:put(Int, 1 + maps:get(Int, Acc, 0), Acc).

-spec count_similarity(M) -> R when
      R :: fun((I, I) -> I),
      I :: integer(),
      M :: #{K :: integer() => V :: integer()}.
count_similarity(Map) ->
    fun (Int, Acc) ->
        Acc + (Int * maps:get(Int, Map, 0))
    end.

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

