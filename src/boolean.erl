% <Erlang Programming>Exercise 2-3: Simple Pattern Matching
%
% Write a module boolean.erl that takes logical expressions and Boolean values (repre-
% sented as the atoms true and false) and returns their Boolean result. The functions
% you write should include b_not/1, b_and/2, b_or/2, and b_nand/2. You should not use
% the logical constructs and, or, and not, but instead use pattern matching to achieve
% your goal.
%
% Richard Kuang
% kuangyel2000@gmail.com
% July 10, 2013


-module(boolean).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

b_not(X) ->
    case X of
        true ->
            false;
        _ ->
            true		
    end.

b_and(X, Y) ->
    case X of
        true ->
            case Y of
                true ->
                    true;
                _ ->
                    false
            end;
        _ ->
            case Y of
                true ->
                    false;
                _->
                    true
            end
    end.

b_or(X, Y) ->
    case X of
        true ->
            true;
        _ ->
            case Y of
                true ->
                    true;
                _ ->
                    false
            end
    end.

b_nand(X, Y) ->
    case X of
        false ->
            false;
        _ ->
            case Y of
                false ->
                    false;
                _ ->
                    true
            end
    end.
