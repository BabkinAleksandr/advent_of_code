-module(p2).
-export([main/0]).
-include_lib("stdlib/include/assert.hrl").
-define(CORRECT_ANSWER, 54885).

main() ->
    Sum = foldl_recursive(fun processor/3, 0, fun get_next_line/0),
    ?assertEqual(Sum, ?CORRECT_ANSWER),
    io:format("~n/2023/d1/P2 result: ~p~n", [Sum]).

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
    WithSubs = make_substitutes(Line),
    case parse_line(WithSubs) of
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

-spec make_substitutes(Line :: string()) -> string().
make_substitutes(Line) ->
    Subs = #{
        "one"=>"o1e",
        "two"=>"t2o",
        "three"=>"th3ee",
        "four"=>"f4ur",
        "five"=>"f5ve",
        "six"=>"s6x",
        "seven"=>"se7en",
        "eight"=>"ei8ht",
        "nine"=>"n9ne"
    },
    maps:fold(fun make_substitute/3, Line, Subs).

-spec make_substitute(Source, Target, Line) -> string() when
      Source :: string(),
      Target :: string(),
      Line :: string().
make_substitute(Source, Target, Line) ->
    R = string:replace(Line, Source, Target, all),
    lists:flatten(R).

