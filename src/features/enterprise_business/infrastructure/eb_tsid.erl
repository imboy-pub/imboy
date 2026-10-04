%%% @doc 注入 ID 的生产实现（`eb_id_port` 的真实现）。
%%%
%%% 依据：铁律 4（domain 不得自行生成 ID）+ EB-02 的 `eb_id_port`。
%%% 实现侧复用仓内 canonical 的 TSID 生成器 `elib_tsid`（64-bit 时间有序整数，
%%% 适配 PostgreSQL BIGINT；JSON 传输按 EntityId 规则转 string）。
%%%
%%% 命名生成器按资源类型分域（`business_identity` / `enterprise_message` /
%%% `enterprise_asset` / `audit` ...），首次使用时惰性注册；`elib_tsid:init/1`
%%% 由应用启动路径（`imboy_app`）完成。未初始化时**不静默兜底**——返回错误，
%%% 避免在节点未就绪时产出可与其它节点冲突的 ID。
-module(eb_tsid).

-moduledoc "注入 ID 的生产实现（eb_id_port 真实现）。".
-behaviour(eb_id_port).

-export([new_id/1]).

%% @doc 生成一个新的 TSID（按 Kind 分域命名生成器）。
-spec new_id(atom()) -> integer().
new_id(Kind) when is_atom(Kind) ->
    ok = ensure_registered(Kind),
    elib_tsid:generate(Kind).

%% @private 惰性注册命名生成器（幂等；注册表在 persistent_term 里，跨进程可见）。
ensure_registered(Kind) ->
    case lists:member(Kind, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(Kind)
    end.
