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

%% 目标行不在队首时先消化无关 ready 行（普通 claim 无 path 过滤）。
claim_ordinary_until(_Conn, _Key, 0, _Tag) ->
    erlang:error({w601_target_job_not_claimed, _Tag});
claim_ordinary_until(Conn, Key, N, Tag) ->
    T = ?X:token(),
    case claim_ordinary(Conn, T) of
        {ok, #{<<"out_path">> := Key} = Row} ->
            {ok, Row};
        {ok, #{<<"out_path">> := Other}} ->
            ok = attachment_gc_repo:v5_ack(Conn, Other, T, true),
            claim_ordinary_until(Conn, Key, N - 1, Tag);
        {error, empty} ->
            claim_ordinary_until(Conn, Key, N - 1, Tag);
        Other ->
            erlang:error({w601_claim_failed, Tag, Other})
    end.

drain_ordinary(Conn) ->
    T = ?X:token(),
    case claim_ordinary(Conn, T) of
        {ok, #{<<"out_path">> := P}} ->
            ok = attachment_gc_repo:v5_ack(Conn, P, T, true),
            drain_ordinary(Conn);
        {error, empty} ->
            ok;
        {error, _} ->
            ok
    end.

reset_ordinary(Conn) ->
    ?X:q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE job_state='leased'"
    ),
    _ = attachment_gc_repo:v5_reap(Conn, 100),
    drain_ordinary(Conn).

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
    case claim_eb(Conn) of
        {ok, #{org := Org, ws := Ws, asset_id := AssetId} = Row} ->
            {ok, Row};
        {ok, #{org := O, ws := W, asset_id := Other, token := T}} ->
            {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)", [O, W, Other, T]),
            claim_eb_for(Conn, Org, Ws, AssetId);
        empty ->
            erlang:error({w601_target_eb_job_not_claimed, AssetId})
    end.

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
    case claim_eb(Conn) of
        {ok, #{org := O, ws := W, asset_id := A, token := T}} ->
            {ok, _, _} = ?X:e(Conn, "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)", [O, W, A, T]),
            drain_eb(Conn);
        empty ->
            ok;
        {error, _} ->
            ok
    end.

reset_eb(Conn) ->
    ?X:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET lease_until = now() - interval '1 s'"
        " WHERE job_state='leased'"
    ),
    _ = ?X:e(Conn, "SELECT fn_v5_reaper_eb(200)", []),
    drain_eb(Conn),
    ok.

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
