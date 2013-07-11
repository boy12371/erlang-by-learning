% learning recursion
% We will traverse a list, filtering out the elements that are not even.

-module(even).
-export([even/1]).

even([]) -> [];
even([Head | Tail]) when is_integer(Head), Head rem 2 == 0 ->
    [Head | even(Tail)];
even([_ | Tail]) ->
    even(Tail).