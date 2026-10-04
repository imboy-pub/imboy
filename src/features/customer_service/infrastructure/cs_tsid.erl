%%% @doc 注入 ID 的生产实现（`cs_id_port` 的真实现；镜像 `eb_tsid` 的口径）。
%%%
%%% 复用仓内 canonical TSID 生成器 `elib_tsid`（64-bit 时间有序整数，适配
%%% PostgreSQL BIGINT）。命名生成器按资源类型分域，首次使用惰性注册；
%%% `elib_tsid:init/1` 由应用启动路径完成，未就绪时不静默兜底。
-module(cs_tsid).

-moduledoc "注入 ID 的生产实现（cs_id_port 真实现，镜像 eb_tsid 口径）。".
-behaviour(cs_id_port).

-export([new_id/1]).

%% @doc 生成一个新的 TSID（按 Kind 分域命名生成器）。
-spec new_id(atom()) -> integer().
new_id(Kind) when is_atom(Kind) ->
    ok = ensure_registered(Kind),
    elib_tsid:generate(Kind).

ensure_registered(Kind) ->
    case lists:member(Kind, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(Kind)
    end.
