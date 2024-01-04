-module(imboy_str).

-export([startswith/2]).


startswith([], _) -> true;
startswith([Ch | Rest1], [Ch | Rest2]) ->
        startswith(Rest1, Rest2);
startswith(_, _) -> false.

% startswith("abc", "abcdef").
