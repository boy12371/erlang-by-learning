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
