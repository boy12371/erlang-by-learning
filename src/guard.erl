% learning guard

-module(guard).
-export([even/1, number/1]).

even(X) when is_integer(X), X rem 2 == 0 ->
    true;
even(X) when is_integer(X), X rem 2 == 1 ->
    false;
even(_X) ->
    {error, 'invalid integer'}.

number(X) when is_integer(X) ->
    integer;
number(X) when is_float(X) ->
    float;
number(_X) ->
    {false, 'not valid integer or fload'}.
