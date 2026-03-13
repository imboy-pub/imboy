-module(imboy_hashid).

%%% @doc Legacy-compatible id helper used by older tests and logic.
%%% Returns numeric binary so it can be used as msg id (binary) and uid/gid (numeric text).

-export([uid/0]).

-spec uid() -> binary().
uid() ->
    NowMs = erlang:system_time(millisecond),
    Suffix = erlang:unique_integer([positive]) rem 100000,
    integer_to_binary(NowMs * 100000 + Suffix).
