%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% Func: mul/0, mul/1
%% Return: 1*1=1        |
%%         1*2=2 2*2=4  |
%%         [ok, ok]     |
%%-----------------------------------------------------------------------

-module(mul).
-export([mul/0, mul/1]).

mul() ->
mul({1, 9}, []).

mul(Y) ->
mul({1, Y}, []).

mul({M1, M2}, List) ->
    case M1 > M2 orelse M1 < 1 of
        true ->
            List;
        _ ->
		mul({M1 + 1, M2}, [mul2(M1) | List])
    end.

mul2(X) when X > 0, is_integer(X) ->
mul2({1, X}, []).

mul2({N1, N2}, New_list) ->
    case N1 > N2 orelse N1 < 1 of
        true ->
            io:format("~n");
        _ ->
            io:format("~w*~w=~w ", [N1, N2, N1*N2]),
            mul2({N1 + 1, N2}, [[N1, N2, N1*N2] | New_list])
    end.