%% @author Richard
%% learning tail recursion


-module(tail_recursion).

%% ====================================================================
%% API functions
%% ====================================================================
-export([bump/1, average/1, even/1, member/2]).


%% ====================================================================
%% bump functions
%% taking a list of integers and adding 1 to every element in the list
%% ====================================================================
bump(List) ->
    bump(List, []).

bump([], List) ->
    reverse(List);
bump([H | T], List) ->
    bump(T, [H + 1 | List]).


%% ====================================================================
%% average functions
%% compute the average of a list of numbers
%% ====================================================================
average(List) ->
    average(List, 0, 0).

average([], _, 0) -> 0;
average([], Sum, Len) ->
    Sum / Len;
average([H | T], Sum, Len) ->
    average(T, Sum + H, Len + 1).


%% ====================================================================
%% even functions
%% traverse a list, filtering out the elements that are not even
%% ====================================================================
even(List) ->
    even(List, []).

even([], List) ->
    reverse(List);
even([H | T], List) when is_integer(H), H rem 2 == 0 ->
    even(T, [H | List]);
even([_ | T], List) ->
    even(T, List).


%% ====================================================================
%% member functions
%% if the list contains at least one element, and we check whether the
%% first element is the one we are looking for
%% ====================================================================
member(X, List) ->
    member(X, List, false).

member(_, [], Bool) ->
    Bool;
member(H, [H | _], Bool) ->
    not(Bool);
member(H, [_ | T], Bool) ->
    member(H, T, Bool).


%% ====================================================================
%% reverse functions
%% an accumulator-based reverse function
%% ====================================================================
reverse(List) ->
    reverse(List, []).

reverse([H | T], List) ->
    reverse(T, [H | List]);
reverse([], List) ->
    List.

