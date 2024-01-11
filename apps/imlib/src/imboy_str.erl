-module(imboy_str).

-export([startswith/2]).


% imboy_str:startswith("abc", "abcdef").
startswith([], _) -> true;
startswith([Ch | Rest1], [Ch | Rest2]) ->
        startswith(Rest1, Rest2);
startswith(_, _) -> false.

