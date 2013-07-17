%% @author Richard
%% @email kuangyel2000@gmail.com
%% @doc @todo learning recursion


-module(recursion).

%% ====================================================================
%% API functions
%% ====================================================================
-export([bump/1, average/1, even/1, member/2]).



%% ====================================================================
%% bump/1 functions
%% taking a list of integers and adding 1 to every element in the list
%% ====================================================================
bump([]) -> [];
bump([Head | Tail]) ->
    [Head + 1 | bump(Tail)].



%% ====================================================================
%% average/1 functions
%% compute the average of a list of numbers
%% ====================================================================
average([]) -> 0;
average(List) ->
    sum(List) / len(List).

sum([]) -> 0;
sum([Head | Tail]) ->
    Head + sum(Tail).

len([]) -> 0;
len([_ | Tail]) ->
    1 + len(Tail).



%% ====================================================================
%% even/1 functions
%% traverse a list, filtering out the elements that are not even
%% ====================================================================
even([]) -> [];
even([Head | Tail]) when is_integer(Head), Head rem 2 == 0 ->
    [Head | even(Tail)];
even([_ | Tail]) ->
    even(Tail).



%% ====================================================================
%% member/2 functions
%% if the list contains at least one element, and we check whether the
%% first element is the one we are looking for
%% ====================================================================
member(_, []) ->
    false;
member(H, [H | _]) ->
    true;
member(H, [_ | T]) ->
    member(H, T).

