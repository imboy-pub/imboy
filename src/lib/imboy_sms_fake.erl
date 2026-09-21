-module(imboy_sms_fake).

%% fake/local 激活短信实现（GZAPP-06；behaviour 见 imboy_sms_provider）。
%%
%% 硬边界：绝不外发——无 HTTP 调用、无第三方依赖；「发送」= 记录进
%% 内存 outbox（ETS bag）+ 脱敏日志。outbox 供测试断言发送目标与次数，
%% 只存于本节点内存，不是持久化通道。
%%
%% 失败注入缝（测试/演练专用）：mobile 以 <<"000">> 开头（国内手机号
%% 不可能的前缀）→ {error, simulated_failure}，用于驱动 D12
%% 「短信失败不回滚企业」路径与 sms_failed 状态。

-behaviour(imboy_sms_provider).

-include("log.hrl").

-export([send_activation/3]).
%% outbox 供测试观察（快照/清空）；非业务路径。
-export([outbox_snapshot/0, outbox_clear/0]).

-define(OUTBOX, imboy_sms_fake_outbox).

%% @doc fake 发送：000 前缀 = 注入失败；其余记录待发并返回 ok。
-spec send_activation(binary(), binary(), binary()) -> ok | {error, binary()}.
send_activation(<<"000", _/binary>> = Mobile, _Token, _OrgName) ->
    _ = ?INFO_LOG([
        owner_activation_sms_fake_simulated_failure,
        {mobile_masked, imboy_mobile:mask(Mobile)}
    ]),
    {error, simulated_failure};
send_activation(Mobile, Token, OrgName) when
    is_binary(Mobile), is_binary(Token), is_binary(OrgName)
->
    ok = ensure_outbox(),
    true = ets:insert(?OUTBOX, {Mobile, Token, OrgName, os:system_time(millisecond)}),
    _ = ?INFO_LOG([
        owner_activation_sms_fake_queued,
        {mobile_masked, imboy_mobile:mask(Mobile)},
        {org_name, OrgName}
    ]),
    ok.

%% @doc outbox 快照（测试断言用）：[{Mobile, Token, OrgName, TsMs}]。
-spec outbox_snapshot() -> [term()].
outbox_snapshot() ->
    ok = ensure_outbox(),
    ets:tab2list(?OUTBOX).

%% @doc 清空 outbox（测试隔离用）。
-spec outbox_clear() -> ok.
outbox_clear() ->
    ok = ensure_outbox(),
    true = ets:delete_all_objects(?OUTBOX),
    ok.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

ensure_outbox() ->
    case ets:info(?OUTBOX, name) of
        undefined ->
            try ets:new(?OUTBOX, [named_table, bag, public, {read_concurrency, true}]) of
                _ -> ok
            catch
                %% 并发竞态：另一进程先建了同名表——正是想要的结果。
                error:badarg -> ok
            end;
        _ ->
            ok
    end.
