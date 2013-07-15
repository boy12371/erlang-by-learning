%% @author Richard
%% kuangyel2000@gmail.com
%% <Erlang Programming>Exercise 2-3: Simple Pattern Matching


-module(demo).

%% ====================================================================
%% API functions
%% ====================================================================
-export([double/1, b_not/1, b_and/2, b_or/2, b_nand/2, area/1, even/1, number/1]).


%% ====================================================================
%% double/1 functions
%% This is a comment. Everything on a line after % is ignored.
%% ====================================================================

double(Value) ->
    times(Value, 2).
	
times(X, Y) ->
    X*Y.


%% ====================================================================
%% b_not/1, b_and/2, b_or/2 and b_nand/2 functions
%% takes logical expressions and Boolean values (repre-sented as the
%% atoms true and false) and returns their Boolean result. The functions
%% you write should include b_not/1, b_and/2, b_or/2, and b_nand/2. You
%% should not use the logical constructs and, or, and not, but instead
%% use pattern matching to achieve your goal.
%% ====================================================================

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


%% ====================================================================
%% area/1 functions
%% This is a comment. Everything on a line after % is ignored.
%% ====================================================================
area({square, Side}) ->
    Side * Side;
area({circle, Radius}) ->
    math:pi() * Radius * Radius;
area({triangle, A, B, C}) ->
    S = (A + B + C)/2,
    math:sqrt(S * (S - A) * (S - B) * (S - C));
area(_Other) ->
    {error, invalid_object}.


%% ====================================================================
%% even/1 functions
%% Try calling the function even with a float or an atom.
%% ====================================================================
even(X) when is_integer(X), X rem 2 == 0 ->
    true;
even(X) when is_integer(X), X rem 2 == 1 ->
    false;
even(_X) ->
    {error, 'invalid integer'}.


%% ====================================================================
%% number/1 functions
%% returns the atom float or integer depending on the argument passed
%% to number/1.
%% ====================================================================
number(X) when is_integer(X) ->
    integer;
number(X) when is_float(X) ->
    float;
number(_X) ->
    {false, 'not valid integer or fload'}.
