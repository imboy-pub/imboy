-module(talk_record_recovery_logic).
-moduledoc "精确本机录音对账；客户端不能指定操作者。".
-export([lookup/2, context/1, replay/4]).

lookup(Uid, Query) when is_integer(Uid), Uid > 0 ->
    case context(Query) of
        {ok, Context} -> talk_record_recovery_ds:lookup(Uid, Context);
        Error -> Error
    end;
lookup(_, _) ->
    {error, not_found}.

%% 当前授权由DS当前行谓词判断；不伪造当前policy/notice事实。
context(Body) -> talk_record_logic:parse_recovery_context(Body).

replay(Uid, Body, Key, Digest) ->
    case context(Body) of
        {ok, Context} -> talk_record_recovery_ds:replay(Uid, Context, Key, Digest);
        Error -> Error
    end.
