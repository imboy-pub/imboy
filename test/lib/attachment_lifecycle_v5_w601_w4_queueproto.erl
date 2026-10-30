%%% @doc W6-01 E 组共享队列协议原语（普通/企业两队列 CAS 协议，worker 只可
%%% claim/ack/reaper）。SQL 形状与封印源 w402_worker_tests /
%%% w403_predecessor_worker 一致；token/SQL 原语复用 w601_w4_context。
-module(attachment_lifecycle_v5_w601_w4_queueproto).

-export([
    claim_ordinary/2,
    claim_ordinary_until/4,
    drain_ordinary/1,
    reset_ordinary/1,
    expire_ordinary/2,
    frozen_ordinary/2,
    claim_eb/1,
    claim_eb_for/4,
    ack_eb/5,
    ack_eb_ok/6,
    enqueue_eb/5,
    expire_eb/3,
    eb_state/3,
    drain_eb/1,
    reset_eb/1,
    frozen_eb/3
]).

-define(X, attachment_lifecycle_v5_w601_w4_context).

%% ------------------------------------------------------- 普通队列协议

claim_ordinary(Conn, Token) ->
    attachment_gc_repo:v5_claim(Conn, Token).

%% 全局 claim 未提供 path 条件；发现非目标时必须回滚 claim，不能 ACK
%% 其他任务。调用方须使用本测试独占连接，不能在已有事务中调用。
claim_ordinary_until(_Conn, _Key, N, Tag) when N =< 0 ->
    erlang:error({w601_target_job_not_claimed, Tag});
claim_ordinary_until(Conn, Key, _N, Tag) ->
    target_claim(
        Conn,
        fun() -> claim_ordinary(Conn, ?X:token()) end,
        fun
            ({ok, #{<<"out_path">> := Path}}) -> Path =:= Key;
            (_) -> false
        end,
        Tag
    ).

drain_ordinary(Conn) ->
    settle_active(Conn, "attachment_object_deletion"),
    require_empty(Conn, "attachment_object_deletion").

reset_ordinary(Conn) ->
    drain_ordinary(Conn).

%% 队列行受迁移 179/190 冻结守卫：DELETE 一律 23510（permanent tombstones），
%% UPDATE 受 identity 不可变 / done 需 completed_at / done 不可复活 / attempts
%% 不回退约束。组间共享同库时前序组可能残留活跃行，隔离断言前按守卫协议把
%% 残留活跃行收敛到 done（completed_at=now()、lease 双字段清空以过
%% ck_v5_*_lease）；require_empty 断言保留——收敛后仍有活跃行照样报错。
settle_active(Conn, Table) ->
    ?X:q(
        Conn,
        "UPDATE " ++ Table ++
            " SET job_state='done', completed_at=now(), lease_token=NULL,"
            " lease_until=NULL WHERE job_state IN ('ready','retry_wait','leased')"
    ),
    ok.

expire_ordinary(Conn, Key) ->
    ?X:q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Key]
    ).

%% 普通队列行的冻结位置快照（worker frozen_location 同形）。
frozen_ordinary(Conn, Key) ->
    {ok, _, [{Driver, Ns, Container, Locator}]} = ?X:e(
        Conn,
        "SELECT storage_driver, storage_namespace, storage_container,"
        " storage_locator_sha256 FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    {ok, I} = attachment_storage:frozen_location(Key, #{
        <<"storage_driver">> => Driver,
        <<"storage_namespace">> => Ns,
        <<"storage_container">> => Container,
        <<"storage_locator_sha256">> => Locator,
        <<"storage_physical_key">> => null
    }),
    I.

%% ------------------------------------------------------- 企业队列协议

claim_eb(Conn) ->
    T = ?X:token(),
    case ?X:e(Conn, "SELECT * FROM fn_v5_claim_eb($1)", [T]) of
        {ok, _, [{Org, Ws, Asset, _Key, _D, _N, _C, _L, _P, Attempts}]} ->
            {ok, #{token => T, attempts => Attempts, org => Org, ws => Ws, asset_id => Asset}};
        {ok, _, []} ->
            empty;
        Other ->
            erlang:error({w601_claim_eb_failed, Other})
    end.

claim_eb_for(Conn, Org, Ws, AssetId) ->
    target_claim(
        Conn,
        fun() -> claim_eb(Conn) end,
        fun
            ({ok, #{org := O, ws := W, asset_id := A}}) ->
                O =:= Org andalso W =:= Ws andalso A =:= AssetId;
            (_) ->
                false
        end,
        {enterprise, Org, Ws, AssetId}
    ).

ack_eb(Conn, Org, Ws, AssetId, Token) ->
    ack_eb_ok(Conn, Org, Ws, AssetId, Token, true).

ack_eb_ok(Conn, Org, Ws, AssetId, Token, Ok) ->
    case ?X:e(Conn, "SELECT fn_v5_ack_eb($1,$2,$3,$4,$5)", [Org, Ws, AssetId, Token, Ok]) of
        {ok, _, [{State}]} -> {ok, State};
        Other -> erlang:error({w601_ack_eb_failed, Other})
    end.

enqueue_eb(Conn, Org, Ws, AssetId, Reason) ->
    case ?X:e(Conn, "SELECT fn_v5_intent_eb_delete($1,$2,$3,$4)", [Org, Ws, AssetId, Reason]) of
        {ok, _, _} -> ok;
        Other -> erlang:error({w601_enqueue_eb_failed, Reason, Other})
    end.

expire_eb(Conn, Org, AssetId) ->
    ?X:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET lease_until = now() - interval '1 s'"
        " WHERE organization_id=$1 AND workspace_id=9102 AND asset_id=$2",
        [Org, AssetId]
    ).

eb_state(Conn, Org, AssetId) ->
    ?X:e(
        Conn,
        "SELECT job_state FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=9102 AND asset_id=$2",
        [Org, AssetId]
    ).

drain_eb(Conn) ->
    settle_active(Conn, "enterprise_asset_delete_queue"),
    require_empty(Conn, "enterprise_asset_delete_queue").

reset_eb(Conn) ->
    drain_eb(Conn).

require_empty(Conn, Table) ->
    Sql =
        "SELECT EXISTS (SELECT 1 FROM " ++ Table ++
            " WHERE job_state IN ('ready','retry_wait','leased'))",
    case ?X:e(Conn, Sql) of
        {ok, _, [{false}]} -> ok;
        {ok, _, [{true}]} -> erlang:error({w601_queue_not_isolated, Table});
        Other -> erlang:error({w601_queue_check_failed, Table, Other})
    end.

target_claim(Conn, Claim, Matches, Tag) ->
    ok = ?X:q(Conn, "BEGIN"),
    try
        Result = Claim(),
        case Matches(Result) of
            true ->
                ok = ?X:q(Conn, "COMMIT"),
                Result;
            false ->
                erlang:error({w601_target_job_not_claimed, Tag, Result})
        end
    catch
        Class:Reason:Stack ->
            ok = ?X:q(Conn, "ROLLBACK"),
            erlang:raise(Class, Reason, Stack)
    end.

%% 企业队列行的冻结位置快照。
frozen_eb(Conn, Org, AssetId) ->
    {ok, _, [{Driver, Ns, Container, Locator, Key}]} = ?X:e(
        Conn,
        "SELECT storage_driver, storage_namespace, storage_container,"
        " storage_locator_sha256, object_key FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=9102 AND asset_id=$2",
        [Org, AssetId]
    ),
    {ok, I} = attachment_storage:frozen_location(Key, #{
        <<"storage_driver">> => Driver,
        <<"storage_namespace">> => Ns,
        <<"storage_container">> => Container,
        <<"storage_locator_sha256">> => Locator,
        <<"storage_physical_key">> => null
    }),
    I.
