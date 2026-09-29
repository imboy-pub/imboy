%%% @doc REVIEW-3 F-2 故障注入 store（test-only；真库套件用）。
%%%
%%% 只覆盖 `append_session_message` 路径触达的两个端口函数：
%%%   * `fetch_session/3` 逐字委派真实现 `cs_pg_store`（会话读取不受影响）；
%%%   * `append_event_in/3` 注入确定性失败——模拟"canonical 事务内事件行
%%%     写失败"，验证消息/审计/事件整体回滚零残留。
%%%
%%% 为什么不用 meck：meck:unload 后 `cs_pg_store:fetch_session/3` 复现 undef
%%%（模块未恢复到可调用状态，run5 实证），而 `store` 端口本身是
%%% `append_session_message` 的显式注入面（与 cs_fake_store 同一惯例），
%%% 注入式故障双确定性等价且无需卸载还原。
-module(cs_f2_fault_store).

-export([fetch_session/3, append_event_in/3]).

fetch_session(OrgId, WorkspaceId, SessionId) ->
    cs_pg_store:fetch_session(OrgId, WorkspaceId, SessionId).

%% 确定性故障：事件行写入永远失败（事务由 canonical tx 裁决整体回滚）。
append_event_in(_Conn, _OrgId, _Event) ->
    {error, {simulated, event_write_down}}.
