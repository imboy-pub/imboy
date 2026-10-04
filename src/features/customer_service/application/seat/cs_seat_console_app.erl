%%% @doc 坐席控制台嵌入的应用层用例（seat-console-embed SC-BE）：管理 CRUD 与
%%% /seat/:id 嵌入面的公开投影。
%%%
%%% 镜像 `cs_widget_app` 的形状与安全合同：
%%%   * 管理面（平台运营）Org/Workspace 由动作表显式要求（organization_id +
%%%     workspace_id），`cs_app_support:tenant/2` 统一门；
%%%   * origin 白名单唯一判定真源在 domain `cs_seat_console:
%%%     normalize_and_dedupe_origins/1`（复用 `cs_widget:normalize_origin/1`
%%%     六禁形状门 + 数量/字节上限 + 保序去重）；非法形状在触达 store 前拒绝；
%%%   * public_seat_console_id 生成口径 = **TSID 十进制 string**（与 widget
%%%     的 R4 同款；注入 fun/0 恒优先，缺省走 id 端口按独立命名域
%%%     `cs_seat_console_public` 生成）；
%%%   * 同一 (Org, Workspace) 至多一个 active 控制台：DB 部分唯一索引裁决，
%%%     23505 归一 `{error, conflict}`（HTTP 409）；
%%%   * PUT 只改 allowed_origins（version + 1）；public id / 作用域 / status
%%%     不可经本用例变更（不在 Updates 投影，显式提交即 422）；可选
%%%     expected_version（正整数）启用乐观并发控制（F-6）：不匹配 →
%%%     `{error, {cas_mismatch, Detail}}`（HTTP 409 + 当前 version），缺省 =
%%%     旧 LWW 行为；
%%%   * 吊销幂等：重复 revoke 返回既有 revoked 行的公开投影（不 404）；
%%%   * /seat/:id 嵌入面：public_seat_console_id **全局**反查，active 门内
%%%     出 `frame_view` 投影（零 org/workspace/secret）；不存在 / revoked
%%%     一律 `{error, seat_console_unavailable}`（404 三态归一，无枚举）。
%%%
%%% 时钟 / ID / store 等事实全部显式注入（照 `cs_app_support` 既有风格）；
%%% 缺注入即 fail-closed，不做隐式推断。TSID 出站由接口层
%%% `cs_http:encode_entity/1` 统一编 string（本层返回 integer，与 widget
%%% 投影同口径）。
-module(cs_seat_console_app).

-moduledoc "坐席控制台嵌入应用层用例（seat-console-embed SC-BE）—— 管理 CRUD 与嵌入面公开投影。".
-export([
    list_consoles/2,
    create_console/2,
    update_console/2,
    revoke_console/2,
    public_frame_console_by_public_id/1
]).

%% ===================================================================
%% 管理面（平台运营）：列表 / 创建 / 更新 / 吊销
%% ===================================================================

-spec list_consoles(integer(), map()) -> {ok, map()} | {error, term()}.
list_consoles(OrgId, Params) when is_map(Params) ->
    %% 控制台是 (Org, Workspace) 级资源：workspace 必填（与平台 GET 契约一致）。
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case cs_app_support:page_cursor(Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, AfterId, Limit} ->
                    list_consoles_page(OrgId, WorkspaceId, AfterId, Limit, Params)
            end
    end;
list_consoles(_OrgId, _Params) ->
    {error, {invalid_argument, list_consoles}}.

list_consoles_page(OrgId, WorkspaceId, AfterId, Limit, Params) ->
    case
        cs_app_support:with_store(Params, fun(Store) ->
            Store:list_seat_consoles_page(OrgId, WorkspaceId, AfterId, Limit)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            Views = [cs_seat_console:public_view(Row) || Row <- Rows],
            cs_app_support:page_view(
                seat_consoles, projection_keys(), Views, Limit, id
            )
    end.

-spec create_console(integer(), map()) -> {ok, map()} | {error, term()}.
create_console(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            create_origins(OrgId, WorkspaceId, Params)
    end;
create_console(_OrgId, _Params) ->
    {error, {invalid_argument, create_console}}.

create_origins(OrgId, WorkspaceId, Params) ->
    case normalize_origins(maps:get(allowed_origins, Params, undefined)) of
        {error, _} = Err ->
            Err;
        {ok, []} ->
            {error, {invalid_argument, allowed_origins}};
        {ok, AllowedOrigins} ->
            case cs_app_support:new_id(cs_seat_console, Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, ConsoleId} ->
                    create_public_id(OrgId, WorkspaceId, AllowedOrigins, ConsoleId, Params)
            end
    end.

create_public_id(OrgId, WorkspaceId, AllowedOrigins, ConsoleId, Params) ->
    case new_public_seat_console_id(Params) of
        {error, _} = Err ->
            Err;
        {ok, PublicId} ->
            Draft = #{
                id => ConsoleId,
                workspace_id => WorkspaceId,
                public_seat_console_id => PublicId,
                allowed_origins => AllowedOrigins,
                created_by_user_id => maps:get(actor_user_id, Params, undefined)
            },
            insert_console(OrgId, Draft, Params)
    end.

insert_console(OrgId, Draft, Params) ->
    case
        cs_app_support:with_store(Params, fun(Store) ->
            Store:insert_seat_console(OrgId, Draft)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            {ok, #{seat_console => cs_seat_console:public_view(Stored)}}
    end.

%% PUT 语义：唯一可编辑键 = allowed_origins（全量提交，domain 全套校验）。
%% public_seat_console_id / status 不可经本用例变更：显式提交即 422
%% （HTTP 面动作表白名单本就不会投影这两个键——本门是纵深防御的 app 层裁决）。
%% F-6（REVIEW-3）：expected_version 可选——提供（正整数）即乐观并发控制，
%% 不匹配 → `{error, {cas_mismatch, Detail}}`（HTTP 409，Detail 携带当前
%% version）；缺省 = 旧 LWW 行为（既有调用方零破坏）。
-spec update_console(integer(), map()) -> {ok, map()} | {error, term()}.
update_console(OrgId, Params) when is_map(Params) ->
    Id = maps:get(id, Params, undefined),
    At = maps:get(at, Params, undefined),
    ExpectedVersion = maps:get(expected_version, Params, undefined),
    ImmutableSubmitted =
        maps:is_key(public_seat_console_id, Params) orelse maps:is_key(status, Params),
    case
        {
            cs_app_support:tenant(OrgId, Params),
            cs_app_support:pos_int(Id),
            cs_app_support:pos_int(At),
            ImmutableSubmitted,
            expected_version_gate(ExpectedVersion)
        }
    of
        {{error, _} = Err, _, _, _, _} ->
            Err;
        {{ok, _WorkspaceId}, false, _, _, _} ->
            {error, {invalid_argument, update_console}};
        {{ok, _WorkspaceId}, _, false, _, _} ->
            {error, {invalid_argument, update_console}};
        {{ok, _WorkspaceId}, _, _, true, _} ->
            {error, {invalid_argument, seat_console_immutable_fields}};
        {{ok, _WorkspaceId}, _, _, _, {error, _} = Err} ->
            Err;
        {{ok, WorkspaceId}, true, true, false, ok} ->
            update_origins(OrgId, WorkspaceId, Id, At, ExpectedVersion, Params)
    end;
update_console(_OrgId, _Params) ->
    {error, {invalid_argument, update_console}}.

%% F-6 形状门：expected_version 缺省合法（LWW）；提供必须是正整数
%% （version 自 1 起）。形状错误在触达 store 前拒绝（422 面）。
expected_version_gate(undefined) -> ok;
expected_version_gate(V) when is_integer(V), V >= 1 -> ok;
expected_version_gate(_) -> {error, {invalid_argument, expected_version}}.

update_origins(OrgId, WorkspaceId, Id, At, ExpectedVersion, Params) ->
    case normalize_origins(maps:get(allowed_origins, Params, undefined)) of
        {error, _} = Err ->
            Err;
        {ok, []} ->
            {error, {invalid_argument, allowed_origins}};
        {ok, AllowedOrigins} ->
            update_console_in(
                OrgId,
                WorkspaceId,
                Id,
                At,
                #{allowed_origins => AllowedOrigins, expected_version => ExpectedVersion},
                Params
            )
    end.

update_console_in(OrgId, WorkspaceId, Id, At, Updates, Params) ->
    case
        cs_app_support:with_store(Params, fun(Store) ->
            Store:update_seat_console(OrgId, WorkspaceId, Id, At, Updates)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            {ok, #{seat_console => cs_seat_console:public_view(Stored)}}
    end.

%% 吊销幂等：store 只翻转 active 行；0 行时 fetch 区分「已是 revoked」（返回
%% 既有行的公开投影，幂等成功）与「不存在/错作用域」（not_found）。
-spec revoke_console(integer(), map()) -> {ok, map()} | {error, term()}.
revoke_console(OrgId, Params) when is_map(Params) ->
    Id = maps:get(id, Params, undefined),
    At = maps:get(at, Params, undefined),
    case
        {
            cs_app_support:tenant(OrgId, Params),
            cs_app_support:pos_int(Id),
            cs_app_support:pos_int(At)
        }
    of
        {{error, _} = Err, _, _} ->
            Err;
        {{ok, _WorkspaceId}, false, _} ->
            {error, {invalid_argument, revoke_console}};
        {{ok, _WorkspaceId}, _, false} ->
            {error, {invalid_argument, revoke_console}};
        {{ok, WorkspaceId}, true, true} ->
            revoke_console_in(OrgId, WorkspaceId, Id, At, Params)
    end;
revoke_console(_OrgId, _Params) ->
    {error, {invalid_argument, revoke_console}}.

revoke_console_in(OrgId, WorkspaceId, Id, At, Params) ->
    case
        cs_app_support:with_store(Params, fun(Store) ->
            Store:revoke_seat_console(OrgId, WorkspaceId, Id, At)
        end)
    of
        ok ->
            fetch_revoked_view(OrgId, WorkspaceId, Id, Params);
        {error, not_found} ->
            %% 幂等面：已是 revoked → 返回既有行；真不存在/错作用域 → not_found。
            fetch_revoked_view(OrgId, WorkspaceId, Id, Params);
        {error, _} = Err ->
            Err
    end.

fetch_revoked_view(OrgId, WorkspaceId, Id, Params) ->
    case
        cs_app_support:with_store(Params, fun(Store) ->
            Store:fetch_seat_console(OrgId, WorkspaceId, Id)
        end)
    of
        {error, not_found} ->
            {error, not_found};
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            case maps:get(status, Stored, undefined) of
                revoked -> {ok, #{seat_console => cs_seat_console:public_view(Stored)}};
                _ -> {error, not_found}
            end
    end.

%% ===================================================================
%% /seat/:public_seat_console_id 嵌入面（零凭证导航，全局反查）
%% ===================================================================

%% @doc 按**全局唯一** public_seat_console_id 反查唯一 active 控制台的嵌入
%% 投影：`#{public_seat_console_id, allowed_origins}`（origin 已归一）。
%% Org/Workspace 由命中行权威派生——浏览器零申报面。
%%
%% 统一错误语义（不存在性不可枚举，三态不区分）：
%%   * 不存在 / status 非 active（revoked kill switch）→
%%     `{error, seat_console_unavailable}`（HTTP 404，handler 直映）；
%%   * 形状门（1..128、[A-Za-z0-9_-]）失败 → `{error, {invalid_argument,
%%     public_seat_console_id}}`（handler 已先 400，纵深防御）。
-spec public_frame_console_by_public_id(map()) -> {ok, map()} | {error, term()}.
public_frame_console_by_public_id(#{public_seat_console_id := PublicId} = Params) when
    is_map(Params)
->
    case cs_seat_console:valid_public_seat_console_id(PublicId) of
        false ->
            {error, {invalid_argument, public_seat_console_id}};
        true ->
            case
                cs_app_support:with_store(Params, fun(Store) ->
                    Store:fetch_seat_console_by_public_id_global(PublicId)
                end)
            of
                {error, not_found} ->
                    {error, seat_console_unavailable};
                {error, _} = Err ->
                    Err;
                {ok, Console} ->
                    case maps:get(status, Console, undefined) of
                        active -> {ok, cs_seat_console:frame_view(Console)};
                        _ -> {error, seat_console_unavailable}
                    end
            end
    end;
public_frame_console_by_public_id(_Params) ->
    {error, {invalid_argument, public_frame_console_by_public_id}}.

%% ===================================================================
%% 内部辅助
%% ===================================================================

normalize_origins(Origins) ->
    cs_seat_console:normalize_and_dedupe_origins(Origins).

projection_keys() ->
    [
        id,
        organization_id,
        workspace_id,
        public_seat_console_id,
        allowed_origins,
        status,
        version,
        created_at,
        updated_at
    ].

%% public_seat_console_id 生成口径（R4 同款）：注入 fun/0 恒优先（测试/内部
%% 合同不变）；缺省走 id 端口按独立命名域 `cs_seat_console_public` 生成，
%% TSID 十进制 string。
new_public_seat_console_id(Params) ->
    case maps:get(new_public_seat_console_id, Params, undefined) of
        Fun when is_function(Fun, 0) -> {ok, Fun()};
        _ ->
            case cs_app_support:new_id(cs_seat_console_public, Params) of
                {ok, Id} when is_integer(Id), Id > 0 -> {ok, integer_to_binary(Id)};
                {error, _} = Err -> Err;
                _ -> {error, {id_generation_failed, cs_seat_console_public}}
            end
    end.
