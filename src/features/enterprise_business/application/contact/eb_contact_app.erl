%%% @doc 企业客户（enterprise contact）与客户资料 / 备注的应用层用例（EB-05）。
%%%
%%% 依据：plan v4.1 §8 EB-05、§2.1 #2/#7、EB-D03 / EB-D04 / EB-D05 / EB-D11。
%%%
%%% **「添加企业好友」只写企业表**（EB-D04）：客户关系 owner 恒为 Organization，
%%% 可选的 `imboy_user_id` 只是企业侧关联字段——本模块**不写个人 `user_friend`**、
%%% 不建个人会话、不写 `msg_c2c` / 私有 `attachment`，也不调用 `user_friend*`。
%%% 员工个人好友是否存在是另一份关系、另一套授权（由 `eb_contact_app_tests` 用
%%% `user_friend` / `msg_c2c` / `attachment` 的行数增量 0 机械证明）。
%%%
%%% **数据访问**：全部经扩展点（`eb_store_port` → `eb_infra_ports:store/0` →
%%% `eb_pg_store`；加密经 `eb_crypto_port` 的装配实现 `eb_managed_crypto`；审计经
%%% `eb_audit_port`）。本模块零 SQL、零 `elib_pg`、不触 `*_repo` / `*_ds`。
%%%
%%% **客户资料加密**：`profile_plaintext` + `key_ref` 由本模块用既有的
%%% `eb_managed_crypto:seal_scoped/3` 封装（AAD 绑定 Org / Workspace / 资源类型 /
%%% 资源 ID），明文一律不入库；缺主密钥 / AAD 不符 fail-closed，不降级为明文。
%%% 渠道标识只落**组织域 HMAC**（`eb_managed_crypto:subject_hmac/4`）+ 掩码，
%%% 不落明文 subject，也不落可字典反查的裸哈希（EB-D04）。
%%%
%%% **确定性资源键（幂等锚）**：同一 `(Org, channel, subject)` 必须落成同一组企业
%%% 行（`enterprise_contact.id` 与 `enterprise_contact_identity.id` 取自组织域
%%% HMAC 摘要的两个不相交 8 字节窗口）。这样「重复添加同一好友」在**第一条
%%% INSERT** 就被唯一键拦下，返回 `{error, {contact_exists, ContactId}}` 且
%%% **零新增行**（不留孤儿 contact）。本模块不新增任何密码学原语：摘要来自既有的
%%% `subject_hmac/4`；派生只做位移与掩码。若后续 store 补上「按 (channel,
%%% subject_hmac) 读渠道标识」的能力，这里可以改回随机 TSID + 显式预检。
%%%
%%% **EB-03R 补齐后的正向能力（本卡 E5-3..E5-9 消费）**
%%%   * `enterprise_contact` 的读/改/列举（`fetch_contact/3`、`update_contact/3`、
%%%     `list_contacts/2`）与 `enterprise_note` 的写入（`insert_note/3`）、
%%%     `enterprise_contact_assignment` 的写入（`insert_contact_assignment/3`）
%%%     都已在 `eb_store_port` 冻结契约内（P3/P4/P7），本模块改走契约 Port。
%%%   * 备注正文由本模块经 `eb_crypto_port:seal_scoped/3` 封好再落库
%%%     （明文不入库、不入日志）；渠道标识经 `subject_hmac/4`（E5-9）。
%%%   * 客户列表分页是**键集**（`after_id` 严格 `id > 游标`），不用 OFFSET。
-module(eb_contact_app).

-export([
    create_contact/2,
    get_contact/2,
    list_contacts/2,
    update_contact/2,
    assign_contact/2,
    append_note/2,
    seal_note_body/3
]).

%% 与迁移 00000115 的 ck_eci_channel 白名单逐字一致（早失败、错误可区分）。
-define(CHANNELS, [<<"imboy">>, <<"wechat">>, <<"phone">>, <<"email">>, <<"other">>]).

%% 与迁移 00000115 的 ck_eca_role 白名单逐字一致（primary 主办 | collaborator 协办）。
-define(ROLES, [<<"primary">>, <<"collaborator">>]).

%% ===================================================================
%% 企业客户（含渠道标识）
%% ===================================================================

%% @doc 新建企业客户 + 渠道标识（「添加企业好友」）。
%%
%% Params：
%%   workspace_id                     必填整数（租户操作范围）
%%   channel                          必填，∈ imboy | wechat | phone | email | other
%%   subject                          必填非空（渠道标识原文；**不落库**，只落 HMAC）
%%   key_ref                          必填（企业托管主密钥引用，用于组织域 HMAC）
%%   display_name / imboy_user_id / subject_mask / created_by_business_identity_id
%%                                    可选
%%   profile_plaintext                可选（客户资料明文；由本模块封装成密文）
%%   profile_cipher / profile_key_version
%%                                    可选（已是密文时直接落库，两者必须成对）
%%   actor_user_id                    可选审计快照
-spec create_contact(integer(), map()) -> {ok, map()} | {error, term()}.
create_contact(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            create_contact_args(OrgId, WorkspaceId, Params)
    end;
create_contact(_OrgId, _Params) ->
    {error, {invalid_argument, create_contact}}.

create_contact_args(OrgId, WorkspaceId, Params) ->
    Channel = maps:get(channel, Params, undefined),
    Subject = maps:get(subject, Params, undefined),
    case lists:member(Channel, ?CHANNELS) of
        false ->
            {error, {unknown_channel, Channel}};
        true ->
            case is_non_empty_binary(Subject) of
                false ->
                    {error, {invalid_subject, Subject}};
                true ->
                    create_contact_keys(OrgId, WorkspaceId, Channel, Subject, Params)
            end
    end.

create_contact_keys(OrgId, WorkspaceId, Channel, Subject, Params) ->
    case
        crypto_port(Params, fun(Crypto) ->
            Crypto:subject_hmac(OrgId, Channel, Subject, key_ref(Params))
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, SubjectHmac} ->
            {ContactId, SubjectId} = derived_ids(SubjectHmac),
            create_contact_rows(
                OrgId, WorkspaceId, Channel, SubjectHmac, ContactId, SubjectId, Params
            )
    end.

create_contact_rows(OrgId, WorkspaceId, Channel, SubjectHmac, ContactId, SubjectId, Params) ->
    case profile_fields(OrgId, WorkspaceId, ContactId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Profile} ->
            Contact = Profile#{
                id => ContactId,
                display_name => maps:get(display_name, Params, undefined),
                imboy_user_id => maps:get(imboy_user_id, Params, undefined),
                created_by_business_identity_id =>
                    maps:get(created_by_business_identity_id, Params, undefined)
            },
            insert_contact(
                OrgId, WorkspaceId, Channel, SubjectHmac, ContactId, SubjectId, Contact, Params
            )
    end.

insert_contact(OrgId, WorkspaceId, Channel, SubjectHmac, ContactId, SubjectId, Contact, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:insert_contact(OrgId, WorkspaceId, Contact)
        end)
    of
        {error, conflict} ->
            %% 同 (Org, channel, subject) 必然派生同 id ⇒ 第一条 INSERT 就是幂等去重点
            {error, {contact_exists, ContactId}};
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            insert_contact_identity(
                OrgId, WorkspaceId, Channel, SubjectHmac, ContactId, SubjectId, Stored, Params
            )
    end.

insert_contact_identity(
    OrgId, WorkspaceId, Channel, SubjectHmac, ContactId, SubjectId, Stored, Params
) ->
    Row = #{
        id => SubjectId,
        contact_id => ContactId,
        channel => Channel,
        subject_hmac => SubjectHmac,
        subject_mask => maps:get(subject_mask, Params, undefined)
    },
    case
        with_store(Params, fun(Store) ->
            Store:insert_contact_identity(OrgId, WorkspaceId, Row)
        end)
    of
        {error, _} = Err ->
            %% 渠道标识落库失败：如实返回，不把已建立的客户行报成成功。
            {error, {contact_identity_not_created, {Channel, ContactId, Err}}};
        {ok, ChannelIdentity} ->
            contact_audit(OrgId, Channel, ChannelIdentity, Stored, Params)
    end.

contact_audit(OrgId, Channel, ChannelIdentity, Stored, Params) ->
    Event = #{
        resource_type => <<"enterprise_contact">>,
        resource_id => maps:get(id, Stored, undefined),
        action => <<"enterprise_contact.create">>,
        business_identity_id => maps:get(created_by_business_identity_id, Stored, undefined),
        actor_user_id => actor_user_id(Params),
        detail => prune_undefined(#{
            <<"channel">> => Channel,
            <<"contact_identity_id">> => maps:get(id, ChannelIdentity, undefined),
            <<"imboy_user_id">> => maps:get(imboy_user_id, Stored, undefined)
        })
    },
    case port(audit, Params) of
        {error, _} = Err ->
            Err;
        {ok, Audit} ->
            case Audit:append(OrgId, Event) of
                {error, Reason} ->
                    {error, {audit_append_failed, Reason}};
                {ok, AuditId} ->
                    {ok, #{
                        contact => Stored,
                        contact_identity => ChannelIdentity,
                        audit_id => AuditId,
                        %% 契约声明：企业好友动作**不产生**个人 friend 行（由测试用
                        %% user_friend 行数增量 0 机械验证）。
                        personal_friend_rows => 0
                    }}
            end
    end.

%% 客户资料：明文经企业托管加密（AAD 绑定 Org/Workspace/资源）后落密文；
%% 已经是密文时要求 key_version 成对出现（与 DB CHECK 同口径）。
profile_fields(OrgId, WorkspaceId, ContactId, Params) ->
    Plaintext = maps:get(profile_plaintext, Params, undefined),
    case Plaintext of
        undefined ->
            %% F-SEC-03（FND-5 贯彻到 contact 域）：无明文 = 不更新 profile。
            %% 客户端密文入口（profile_cipher/profile_key_version）已从动作表
            %% 删除并由 HTTP 守卫 422——profile 只经服务端托管密钥封装。
            {ok, #{}};
        Value when is_binary(Value) ->
            Aad = scope(OrgId, WorkspaceId, <<"enterprise_contact">>, ContactId),
            case
                crypto_port(Params, fun(Crypto) ->
                    Crypto:seal_scoped(Aad, Value, key_ref(Params))
                end)
            of
                {ok, Sealed} ->
                    {ok, #{
                        profile_cipher => maps:get(cipher, Sealed, undefined),
                        profile_key_version => maps:get(key_version, Sealed, undefined)
                    }};
                {error, _} = Err ->
                    Err
            end;
        Other ->
            {error, {invalid_profile_plaintext, Other}}
    end.

%% ===================================================================
%% 客户备注
%% ===================================================================

%% @doc 追加客户备注（企业托管密文正文）。
%%
%% 校验顺序：租户参数 → contact_id 类型 → 正文形状 → 客户必须在本租户内且 active
%% → 经契约 `eb_crypto_port:seal_scoped/3` 封好正文（AAD 绑定
%% Org/Workspace/`enterprise_note`/资源 ID）→ `eb_store_port:insert_note/3` 落库
%% → 审计。**明文只存在于调用栈**：不入库、不入日志、不进返回值（返回值里只有
%% 密文封装）。
%%
%% Params：
%%   workspace_id                     必填整数
%%   contact_id                       必填整数
%%   body_plaintext                   必填明文（FND-5，RULING-2026-09-15 §七：
%%                                    「HTTP 接受业务明文，服务端经 provider 加密，
%%                                    store 只接收密文」—— 客户端密文提交路径已删除）
%%   business_identity_id / actor_user_id / key_ref / id   可选
-spec append_note(integer(), map()) -> {ok, map()} | {error, term()}.
append_note(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            append_note_args(OrgId, WorkspaceId, Params)
    end;
append_note(_OrgId, _Params) ->
    {error, {invalid_argument, append_note}}.

append_note_args(OrgId, WorkspaceId, Params) ->
    ContactId = maps:get(contact_id, Params, undefined),
    case is_pos_int(ContactId) of
        false ->
            {error, {invalid_contact_id, ContactId}};
        true ->
            case note_body(Params) of
                {error, _} = Err ->
                    Err;
                {ok, Body} ->
                    append_note_contact(OrgId, WorkspaceId, ContactId, Body, Params)
            end
    end.

append_note_contact(OrgId, WorkspaceId, ContactId, Body, Params) ->
    case fetch_contact_or_error(OrgId, WorkspaceId, ContactId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Contact} ->
            case maps:get(status, Contact, undefined) of
                active ->
                    append_note_seal(OrgId, WorkspaceId, ContactId, Body, Params);
                OtherStatus ->
                    {error, {contact_not_active, OtherStatus}}
            end
    end.

append_note_seal(OrgId, WorkspaceId, ContactId, Body, Params) ->
    case note_id(Body, Params) of
        {error, _} = Err ->
            Err;
        {ok, NoteId} ->
            case note_cipher(OrgId, WorkspaceId, NoteId, Body, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Cipher, KeyVersion, Sealed} ->
                    Note = #{
                        id => NoteId,
                        contact_id => ContactId,
                        business_identity_id => maps:get(business_identity_id, Params, undefined),
                        actor_user_id => actor_user_id(Params),
                        body_cipher => Cipher,
                        body_key_version => KeyVersion
                    },
                    append_note_store(OrgId, WorkspaceId, ContactId, Note, Sealed, Params)
            end
    end.

append_note_store(OrgId, WorkspaceId, ContactId, Note, Sealed, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:insert_note(OrgId, WorkspaceId, Note)
        end)
    of
        {error, _} = Err ->
            %% 落库失败：如实返回，不报成功、不留半写行。
            {error, {note_not_created, Err}};
        {ok, Stored} ->
            append_note_audit(OrgId, ContactId, Stored, Sealed, Params)
    end.

append_note_audit(OrgId, ContactId, Stored, Sealed, Params) ->
    Event = #{
        resource_type => <<"enterprise_note">>,
        resource_id => maps:get(id, Stored, undefined),
        action => <<"enterprise_note.create">>,
        business_identity_id => maps:get(business_identity_id, Stored, undefined),
        actor_user_id => actor_user_id(Params),
        detail => prune_undefined(#{
            <<"contact_id">> => ContactId,
            <<"body_key_version">> => maps:get(body_key_version, Stored, undefined)
        })
    },
    case port(audit, Params) of
        {error, _} = Err ->
            Err;
        {ok, Audit} ->
            case Audit:append(OrgId, Event) of
                {error, Reason} ->
                    {error, {audit_append_failed, Reason}};
                {ok, AuditId} ->
                    %% 返回值里只有密文封装，绝不含明文。
                    {ok,
                        prune_undefined(#{
                            note => Stored,
                            sealed => Sealed,
                            audit_id => AuditId
                        })}
            end
    end.

%% 备注正文形状（FND-5，RULING-2026-09-15 §七）：**只**接受 `body_plaintext`。
%% 客户端提交 `body_cipher`/`body_key_version` 的旧路径已删除 —— 那会让调用方
%% 自带密文与密钥版本进入 store，绕过「服务端经 provider 加密」的边界；
%% 显式拒绝（fail-closed）而不是静默忽略。
note_body(Params) ->
    case
        {
            maps:get(body_plaintext, Params, undefined),
            maps:get(body_cipher, Params, undefined) =/= undefined orelse
                maps:get(body_key_version, Params, undefined) =/= undefined
        }
    of
        {Plaintext, false} when is_binary(Plaintext) ->
            {ok, #{mode => plaintext, plaintext => Plaintext}};
        {_Other, false} ->
            {error, {invalid_body_plaintext, maps:get(body_plaintext, Params, undefined)}};
        _ClientSuppliedCipher ->
            {error, client_cipher_not_accepted}
    end.

%% 备注 ID：由本模块生成（AAD 与之一致）。FND-5 后只有明文路径——
%% 「客户端自带密文 + 既有 note_id」的旁路已随 cipher 模式一并删除。
note_id(#{mode := plaintext}, Params) ->
    new_id(enterprise_note, Params).

note_cipher(OrgId, WorkspaceId, NoteId, #{mode := plaintext, plaintext := Plaintext}, Params) ->
    case seal_scoped(OrgId, WorkspaceId, <<"enterprise_note">>, NoteId, Plaintext, Params) of
        {ok, Sealed} ->
            {ok, maps:get(cipher, Sealed, undefined), maps:get(key_version, Sealed, undefined),
                Sealed};
        {error, _} = Err ->
            Err
    end.

%% 备注正文封装（E5-9：走契约的 `eb_crypto_port:seal_scoped/3`）。
seal_scoped(OrgId, WorkspaceId, ResourceType, ResourceId, Plaintext, Params) ->
    Aad = scope(OrgId, WorkspaceId, ResourceType, ResourceId),
    crypto_port(Params, fun(Crypto) ->
        Crypto:seal_scoped(Aad, Plaintext, key_ref(Params))
    end).

%% @doc 用企业托管加密封装备注正文（AAD 绑定 Org/Workspace/`enterprise_note`/资源 ID），
%% 返回可供 `append_note/2` 使用的密文形状。供 EB-09 的 handler 复用。
-spec seal_note_body(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
seal_note_body(OrgId, WorkspaceId, Params) when is_map(Params) ->
    case {is_pos_int(OrgId), is_pos_int(WorkspaceId)} of
        {true, true} ->
            seal_note_body_in(OrgId, WorkspaceId, Params);
        _NotTenant ->
            {error, {invalid_tenant, {OrgId, WorkspaceId}}}
    end;
seal_note_body(_OrgId, _WorkspaceId, _Params) ->
    {error, {invalid_argument, seal_note_body}}.

seal_note_body_in(OrgId, WorkspaceId, Params) ->
    Plaintext = maps:get(body_plaintext, Params, undefined),
    case is_binary(Plaintext) of
        false ->
            {error, {invalid_body_plaintext, Plaintext}};
        true ->
            case new_id(enterprise_note, Params) of
                {error, _} = Err ->
                    Err;
                {ok, NoteId} ->
                    Aad = scope(OrgId, WorkspaceId, <<"enterprise_note">>, NoteId),
                    case
                        crypto_port(Params, fun(Crypto) ->
                            Crypto:seal_scoped(Aad, Plaintext, key_ref(Params))
                        end)
                    of
                        {ok, Sealed} ->
                            {ok, #{
                                id => NoteId,
                                sealed => Sealed,
                                body_cipher => maps:get(cipher, Sealed, undefined),
                                body_key_version => maps:get(key_version, Sealed, undefined)
                            }};
                        {error, _} = Err ->
                            Err
                    end
            end
    end.

%% ===================================================================
%% 客户详情 / 列举 / 更新（§5.1 GET、PATCH contacts）
%% ===================================================================

%% @doc 客户详情（§5.1 GET /contacts/{id}）。
%%
%% 租户作用域由 store 的**同语句**租户键裁决：跨 Org / 跨 Workspace / 不存在
%% 一律 `{error, {contact_not_found, Id}}`（不区分，避免被用来枚举他 Org 主键）。
-spec get_contact(integer(), map()) -> {ok, map()} | {error, term()}.
get_contact(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            get_contact_in(OrgId, WorkspaceId, Params)
    end;
get_contact(_OrgId, _Params) ->
    {error, {invalid_argument, get_contact}}.

get_contact_in(OrgId, WorkspaceId, Params) ->
    ContactId = maps:get(contact_id, Params, undefined),
    case is_pos_int(ContactId) of
        false -> {error, {invalid_contact_id, ContactId}};
        true -> fetch_contact_or_error(OrgId, WorkspaceId, ContactId, Params)
    end.

%% @doc 列举本 Org 的客户（§5.1 GET /contacts）。
%%
%% 只返回本 Org 行（SQL 同语句带 Org + Workspace 归属）；跨 Org 一律空列表。
%% 分页为**键集**语义：`after_id` 严格 `id > 游标`、`limit` 截断，不用 OFFSET。
-spec list_contacts(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_contacts(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            list_contacts_in(OrgId, WorkspaceId, Params)
    end;
list_contacts(_OrgId, _Params) ->
    {error, {invalid_argument, list_contacts}}.

list_contacts_in(OrgId, WorkspaceId, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:list_contacts(OrgId, WorkspaceId)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            {ok, keyset_page(Rows, Params)}
    end.

%% @doc 客户资料更新（§5.1 PATCH /contacts/{id}）。
%%
%% **白名单**：只有 `display_name` / `profile_cipher` / `profile_key_version`
%% 可改；`id` / `organization_id` / `status` / `imboy_user_id` / `version`
%% 传了也不生效（不可变字段无法经此入口被改动，改动只留审计）。
%%
%% 跨 Org / 跨 Workspace / 不存在 ⇒ `contact_not_found` 且**零副作用**（先按
%% 租户读一次，读不到就不进入写路径）。
-spec update_contact(integer(), map()) -> {ok, map()} | {error, term()}.
update_contact(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            update_contact_in(OrgId, WorkspaceId, Params)
    end;
update_contact(_OrgId, _Params) ->
    {error, {invalid_argument, update_contact}}.

update_contact_in(OrgId, WorkspaceId, Params) ->
    ContactId = maps:get(contact_id, Params, undefined),
    case is_pos_int(ContactId) of
        false ->
            {error, {invalid_contact_id, ContactId}};
        true ->
            case update_patch(OrgId, WorkspaceId, ContactId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Patch} ->
                    case fetch_contact_or_error(OrgId, WorkspaceId, ContactId, Params) of
                        {error, _} = Err ->
                            Err;
                        {ok, _Contact} ->
                            update_contact_store(OrgId, WorkspaceId, ContactId, Patch, Params)
                    end
            end
    end.

update_patch(OrgId, WorkspaceId, ContactId, Params) ->
    case display_patch(Params) of
        {error, _} = Err ->
            Err;
        {ok, Display} ->
            case profile_patch(OrgId, WorkspaceId, ContactId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Profile} ->
                    Patch = maps:merge(Display, Profile),
                    case map_size(Patch) of
                        0 -> {error, empty_patch};
                        _ -> {ok, Patch#{id => ContactId}}
                    end
            end
    end.

display_patch(Params) ->
    case maps:get(display_name, Params, undefined) of
        undefined -> {ok, #{}};
        Name when is_binary(Name), byte_size(Name) > 0 -> {ok, #{display_name => Name}};
        Other -> {error, {invalid_display_name, Other}}
    end.

%% 资料密文：明文经契约封装；已是密文时 key_version 必须成对（与 DB CHECK 同口径）。
profile_patch(OrgId, WorkspaceId, ContactId, Params) ->
    case maps:get(profile_plaintext, Params, undefined) of
        undefined ->
            profile_patch_cipher(Params);
        Plaintext when is_binary(Plaintext) ->
            case
                seal_scoped(
                    OrgId, WorkspaceId, <<"enterprise_contact">>, ContactId, Plaintext, Params
                )
            of
                {ok, Sealed} ->
                    {ok, #{
                        profile_cipher => maps:get(cipher, Sealed, undefined),
                        profile_key_version => maps:get(key_version, Sealed, undefined)
                    }};
                {error, _} = Err ->
                    Err
            end;
        Other ->
            {error, {invalid_profile_plaintext, Other}}
    end.

profile_patch_cipher(Params) ->
    Cipher = maps:get(profile_cipher, Params, undefined),
    KeyVersion = maps:get(profile_key_version, Params, undefined),
    case {Cipher, KeyVersion} of
        {undefined, undefined} ->
            {ok, #{}};
        {undefined, _OrphanVersion} ->
            {error, missing_profile_cipher};
        {_Cipher, undefined} ->
            {error, missing_profile_key_version};
        {_Cipher, Version} when is_integer(Version), Version >= 1 ->
            {ok, #{profile_cipher => Cipher, profile_key_version => Version}};
        {_Cipher, Other} ->
            {error, {invalid_profile_key_version, Other}}
    end.

update_contact_store(OrgId, WorkspaceId, ContactId, Patch, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:update_contact(OrgId, WorkspaceId, Patch)
        end)
    of
        {error, not_found} ->
            {error, {contact_not_found, ContactId}};
        {error, conflict} ->
            {error, {contact_update_conflict, ContactId}};
        {error, _} = Err ->
            Err;
        {ok, Updated} ->
            update_contact_audit(OrgId, Updated, Patch, Params)
    end.

update_contact_audit(OrgId, Updated, Patch, Params) ->
    Fields = [atom_to_binary(Key, utf8) || Key <- lists:sort(maps:keys(Patch)), Key =/= id],
    Event = #{
        resource_type => <<"enterprise_contact">>,
        resource_id => maps:get(id, Updated, undefined),
        action => <<"enterprise_contact.update">>,
        business_identity_id => maps:get(created_by_business_identity_id, Updated, undefined),
        actor_user_id => actor_user_id(Params),
        detail => #{<<"fields">> => iolist_to_binary(lists:join(<<",">>, Fields))}
    },
    case port(audit, Params) of
        {error, _} = Err ->
            Err;
        {ok, Audit} ->
            case Audit:append(OrgId, Event) of
                {error, Reason} -> {error, {audit_append_failed, Reason}};
                {ok, AuditId} -> {ok, Updated#{audit_id => AuditId}}
            end
    end.

%% ===================================================================
%% 客户 ↔ 业务身份经办（§4.1 / §5.1 POST contacts/:id/assignment）
%% ===================================================================

%% @doc 把客户分配给某个业务身份（primary 主办 / collaborator 协办）。
%%
%% 租户作用域双重判定：客户与业务身份**都**必须在本 Org + 本 Workspace 内
%% （先按租户各读一次，任一读不到即拒绝且零写入）。同一客户同时最多一个
%% active `primary`（DB 唯一索引 `uq_eca_primary_contact` 裁决）。
-spec assign_contact(integer(), map()) -> {ok, map()} | {error, term()}.
assign_contact(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            assign_contact_in(OrgId, WorkspaceId, Params)
    end;
assign_contact(_OrgId, _Params) ->
    {error, {invalid_argument, assign_contact}}.

assign_contact_in(OrgId, WorkspaceId, Params) ->
    ContactId = maps:get(contact_id, Params, undefined),
    IdentityId = maps:get(business_identity_id, Params, undefined),
    Role = maps:get(role, Params, <<"primary">>),
    case {is_pos_int(ContactId), is_pos_int(IdentityId), lists:member(Role, ?ROLES)} of
        {false, _, _} ->
            {error, {invalid_contact_id, ContactId}};
        {_, false, _} ->
            {error, {invalid_business_identity_id, IdentityId}};
        {_, _, false} ->
            {error, {invalid_role, Role}};
        {true, true, true} ->
            case fetch_contact_or_error(OrgId, WorkspaceId, ContactId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, _Contact} ->
                    assign_contact_identity(
                        OrgId, WorkspaceId, ContactId, IdentityId, Role, Params
                    )
            end
    end.

assign_contact_identity(OrgId, WorkspaceId, ContactId, IdentityId, Role, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_identity(OrgId, WorkspaceId, IdentityId)
        end)
    of
        {error, not_found} ->
            {error, {identity_not_found, IdentityId}};
        {error, _} = Err ->
            Err;
        {ok, _Identity} ->
            assign_contact_insert(OrgId, WorkspaceId, ContactId, IdentityId, Role, Params)
    end.

assign_contact_insert(OrgId, WorkspaceId, ContactId, IdentityId, Role, Params) ->
    case new_id(enterprise_contact_assignment, Params) of
        {error, _} = Err ->
            Err;
        {ok, AssignmentId} ->
            Row = #{
                id => AssignmentId,
                contact_id => ContactId,
                business_identity_id => IdentityId,
                role => Role,
                assigned_by => actor_user_id(Params)
            },
            case
                with_store(Params, fun(Store) ->
                    Store:insert_contact_assignment(OrgId, WorkspaceId, Row)
                end)
            of
                {error, conflict} ->
                    {error, {contact_assignment_conflict, ContactId}};
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    assign_contact_audit(OrgId, Stored, Params)
            end
    end.

assign_contact_audit(OrgId, Stored, Params) ->
    Event = #{
        resource_type => <<"enterprise_contact_assignment">>,
        resource_id => maps:get(id, Stored, undefined),
        action => <<"enterprise_contact_assignment.create">>,
        business_identity_id => maps:get(business_identity_id, Stored, undefined),
        actor_user_id => actor_user_id(Params),
        detail => prune_undefined(#{
            <<"contact_id">> => maps:get(contact_id, Stored, undefined),
            <<"role">> => maps:get(role, Stored, undefined)
        })
    },
    case port(audit, Params) of
        {error, _} = Err ->
            Err;
        {ok, Audit} ->
            case Audit:append(OrgId, Event) of
                {error, Reason} ->
                    {error, {audit_append_failed, Reason}};
                {ok, AuditId} ->
                    {ok, Stored#{audit_id => AuditId}}
            end
    end.

%% ===================================================================
%% 内部辅助：端口 / 租户 / 资源键 / 审计
%% ===================================================================

port(Key, Params) ->
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(Key)
    end.

with_store(Params, Fun) ->
    case port(store, Params) of
        {ok, Store} -> Fun(Store);
        {error, _} = Err -> Err
    end.

crypto_port(Params, Fun) ->
    case port(crypto, Params) of
        {ok, Crypto} -> Fun(Crypto);
        {error, _} = Err -> Err
    end.

%% F6（RULING-2026-09-15 §七）主密钥装配：显式注入（map 形态的测试/内部合同）
%% 原样优先；缺省经 `eb_env_keyring` 从服务端 env 解析 active key_ref。env 缺失
%% 时得 undefined，Crypto 面照旧 `{error, missing_key}` fail-closed（500 面）。
key_ref(Params) ->
    eb_env_keyring:resolve_key_ref(maps:get(key_ref, Params, undefined)).

new_id(Kind, Params) ->
    case port(id, Params) of
        {ok, IdPort} ->
            try
                {ok, IdPort:new_id(Kind)}
            catch
                Class:Reason ->
                    {error, {id_generation_failed, Kind, {Class, Reason}}}
            end;
        {error, _} = Err ->
            Err
    end.

scope(OrgId, WorkspaceId, ResourceType, ResourceId) ->
    #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        resource_type => ResourceType,
        resource_id => ResourceId
    }.

%% 同 (Org, channel, subject) ⇒ 同 (contact_id, subject_id)：两个不相交 8 字节窗口，
%% 掩到 63 位（BIGINT 安全）并置最低位，保证非零且确定性。
derived_ids(SubjectHmac) ->
    Bytes = binary:decode_hex(SubjectHmac),
    {id_window(Bytes, 0), id_window(Bytes, 8)}.

id_window(Bytes, Offset) ->
    <<_:Offset/binary, Chunk:8/binary, _/binary>> = Bytes,
    (binary:decode_unsigned(Chunk) band 16#7FFFFFFFFFFFFFFF) bor 1.

%% actor 只作审计快照：显式 actor_user_id → assigned_by → created_by_user_id。
actor_user_id(Params) ->
    first_defined([
        maps:get(actor_user_id, Params, undefined),
        maps:get(assigned_by, Params, undefined),
        maps:get(created_by_user_id, Params, undefined)
    ]).

first_defined([]) ->
    undefined;
first_defined([undefined | Rest]) ->
    first_defined(Rest);
first_defined([Value | _Rest]) ->
    Value.

%% 审计 detail 是 jsonb：不允许携带 undefined（也不得放明文客户资料）。
prune_undefined(Map) ->
    maps:filter(fun(_Key, Value) -> Value =/= undefined end, Map).

tenant(OrgId, Params) ->
    case is_pos_int(OrgId) of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case maps:get(workspace_id, Params, undefined) of
                Ws when is_integer(Ws), Ws > 0 -> {ok, Ws};
                Other -> {error, {invalid_workspace_id, Other}}
            end
    end.

%% 客户读取 + 租户错误归一：跨 Org / 跨 Workspace / 不存在一律 contact_not_found。
fetch_contact_or_error(OrgId, WorkspaceId, ContactId, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_contact(OrgId, WorkspaceId, ContactId)
        end)
    of
        {error, not_found} -> {error, {contact_not_found, ContactId}};
        {error, _} = Err -> Err;
        {ok, Contact} -> {ok, Contact}
    end.

%% 键集分页（非 offset）：按 id 升序 → 严格 `id > after_id` → `limit` 截断。
keyset_page(Rows, Params) ->
    Sorted = lists:sort(
        fun(A, B) -> maps:get(id, A, 0) =< maps:get(id, B, 0) end,
        Rows
    ),
    After = maps:get(after_id, Params, undefined),
    Filtered =
        case is_pos_int(After) of
            true -> [Row || Row <- Sorted, maps:get(id, Row, 0) > After];
            false -> Sorted
        end,
    case maps:get(limit, Params, undefined) of
        Limit when is_integer(Limit), Limit >= 0 -> lists:sublist(Filtered, Limit);
        _NoLimit -> Filtered
    end.

is_pos_int(Value) ->
    is_integer(Value) andalso Value > 0.

is_non_empty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.
