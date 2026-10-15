%%% Durable purge intents: capture keys before metadata deletion, never perform I/O in its transaction.
%%% W4-02：drain 消费改走迁移 179 统一 CAS 协议（fn_v5_claim_eb/ack_eb——
%%% SKIP LOCKED、lease 120s、attempts<8、7 档退避、max_attempts→blocked），
%%% 不再裸 SELECT+DELETE 行；旧 drain 语义（删除成功才移除行）由 done 终态
%%% 等价承载（done 行保留 = retired key 墓碑）。
-module(eb_pg_asset_delete_queue).
-moduledoc "持久化 purge intent 队列 —— 元数据删除前捕获 key，事务内绝不做 I/O。".
-export([enqueue_in/5, drain/3, drain/4, claim/1, ack/5, reap/1]).

enqueue_in(Conn, Org, Ws, MessageIds, AssetIds) ->
    case
        elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key)"
                " SELECT organization_id,workspace_id,id,object_key FROM enterprise_asset"
                " WHERE organization_id=$1 AND workspace_id=$2"
                " AND (message_id=ANY($3::bigint[]) OR id=ANY($4::bigint[]))"
                " ON CONFLICT (organization_id,workspace_id,asset_id) DO NOTHING"
            >>,
            [Org, Ws, MessageIds, AssetIds]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> throw({rollback, {object_intent_failed, Reason}})
    end.

drain(Org, Ws, Limit) ->
    drain(Org, Ws, Limit, elib_uuid:gen_v7()).

%% 消费至多 Limit 个 job（租约 CAS；删除成功 ack done、失败 ack 退避/终止）。
drain(Org, Ws, Limit, Token) when Limit > 0 ->
    case claim(Token) of
        {ok, Row} ->
            case process_job(Org, Ws, Row, Token) of
                ok ->
                    drain(Org, Ws, Limit - 1, Token);
                {error, delete_failed} ->
                    {ok, []};
                {error, _} = Error ->
                    Error
            end;
        {error, empty} ->
            {ok, []};
        {error, Reason} ->
            {error, {object_queue_query_failed, Reason}}
    end;
drain(_Org, _Ws, _Limit, _Token) ->
    {ok, []}.

process_job(_RequestedOrg, _RequestedWs, Row, Token) ->
    Org = maps:get(<<"out_organization_id">>, Row),
    Ws = maps:get(<<"out_workspace_id">>, Row),
    AssetId = maps:get(<<"out_asset_id">>, Row),
    Key = maps:get(<<"out_object_key">>, Row),
    Frozen = maps:from_list([
        {K, maps:get(<<"out_", K/binary>>, Row, null)}
     || K <-
            [
                <<"storage_driver">>,
                <<"storage_namespace">>,
                <<"storage_container">>,
                <<"storage_locator_sha256">>
            ]
    ]),
    Result =
        try
            case attachment_storage:frozen_location(Key, Frozen) of
                {ok, I} when is_map(I) -> attachment_storage:delete_snapshot(I);
                _ -> {error, location_unverified}
            end
        catch
            _:_ -> {error, worker_crash}
        end,
    AckOk = Result =:= ok orelse Result =:= {error, not_found},
    case ack(Org, Ws, AssetId, Token, AckOk) of
        ok when AckOk -> ok;
        ok -> {error, delete_failed};
        {error, R} -> {error, {ack_failed, R}}
    end.

%% 企业队列 claim（租约 CAS；fn 返回单 job 或空集）。
claim(Token) ->
    case
        elib_pg:query(
            <<
                "SELECT out_organization_id, out_workspace_id, out_asset_id,"
                " out_object_key, out_storage_driver, out_storage_namespace,"
                " out_storage_container, out_storage_locator_sha256, out_attempts"
                " FROM fn_v5_claim_eb($1)"
            >>,
            [Token]
        )
    of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, empty};
        {error, R} -> {error, R}
    end.

ack(Org, Ws, AssetId, Token, Ok) ->
    case
        elib_pg:query(
            <<"SELECT fn_v5_ack_eb($1, $2, $3, $4, $5)">>,
            [Org, Ws, AssetId, Token, Ok]
        )
    of
        {ok, [#{<<"fn_v5_ack_eb">> := <<"done">>}]} when Ok =:= true -> ok;
        {ok, [#{<<"fn_v5_ack_eb">> := State}]} when
            Ok =:= false, (State =:= <<"retry_wait">> orelse State =:= <<"blocked">>)
        ->
            ok;
        {ok, [#{<<"fn_v5_ack_eb">> := State}]} ->
            {error, {ack_rejected, State}};
        {ok, Rows} ->
            {error, {invalid_ack_result, Rows}};
        {error, R} ->
            {error, R}
    end.

reap(Limit) ->
    case elib_pg:query(<<"SELECT fn_v5_reaper_eb($1)">>, [Limit]) of
        {ok, _} -> {ok, 0};
        {error, R} -> {error, R}
    end.
