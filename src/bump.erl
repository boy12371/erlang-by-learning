% learning recursion
%
% bump([1, 2, 3] => [1 + 1 | bump([2, 3])
%     1 + 1 => 2
%     bump([2, 3]) => [2 + 1 | bump([3])
%         2 + 1 => 3
%         bump([3]) => [3 + 1 | bump([])
%             3 + 1 => 4
%             bump([]) => []
%             [4 | []] => [4]
%         [4] <=
%         [3 | [4]] => [3, 4]
%     [3, 4] <=
%     [2 | [3, 4]] => [2, 3, 4]
% [2, 3, 4] <=
%

-module(bump).
-export([bump/1]).

bump([]) -> [];
bump([Head | Tail]) ->
    [Head + 1 | bump(Tail)].