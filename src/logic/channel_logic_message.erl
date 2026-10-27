-module(channel_logic_message).
-compile([nowarn_deprecated_catch]).
%% Internal channel_content message/application logic.

-export([channel_transfer/1]).
-export([message_transfer/1]).
-export([create_channel/4]).
%% W04.c（channel-group-ux v2.2 §6.4 C1）：带受理回执的频道创建。
-export([create_channel_with_receipt/7]).
-export([get_channel/2]).
-export([get_channel_by_custom_id/2]).
-export([update_channel/3]).
-export([delete_channel/2]).
%% GZAPP-02/G4：频道归档/恢复
-export([archive_channel/2]).
-export([restore_channel/2]).
-export([publish_message/5]).
-export([publish_message/6]).
-export([get_messages/4, get_messages_page/4, get_message/3]).
-export([mark_as_read/3]).
-export([mark_viewed/3]).
-export([search_channels/2]).
-export([get_discover_channels/1]).
-export([add_admin/4]).
-export([remove_admin/3]).
-export([pin_message/3]).
-export([delete_message/2]).
-export([revoke_message/3]).
-export([edit_message/4]).
-export([get_admins/1]).
-export([update_admin_role/4]).
-export([attach_my_reactions/2]).

-include("log.hrl").

-ifdef(TEST).
%% 帖子图片归属校验直测（发布链 mock 面大，校验逻辑独立直测）。
-export([validate_post_images/2]).
-endif.

%% 用户侧出站安全列白名单（P1-7b 宽列治理）：替代 <<"*">>，
%% 防止 channel 表未来新增敏感列随行自动泄漏进 payload / WS 通知。
%% 内部状态列 status（审核/软删，查询恒过滤 status=1）不下发；
%% 向本清单新增列前必须评估其用户侧敏感性。
%% 注意与 channel_logic_subscription.erl 中的同名宏保持一致。
-define(CHANNEL_SAFE_COLUMNS, <<
    "id,name,description,avatar,custom_id,creator_uid,subscriber_count,"
    "is_verified,tags,visibility,access_type,join_policy,created_at,updated_at"
>>).

-spec channel_transfer(map()) -> map().
channel_transfer(Channel) when is_map(Channel) ->
    channel_logic_common:channel_transfer(Channel).

-spec message_transfer(map()) -> map().
message_transfer(Message) when is_map(Message) ->
    Message.

-spec create_channel(integer(), binary(), map(), integer()) ->
    {ok, map()} | {error, binary()}.
create_channel(Uid, Name, Opts, MaxChannels) ->
    Options = Opts#{max_channels => MaxChannels},
    case maps:get(custom_id, Opts, undefined) of
        CustomId when is_binary(CustomId), CustomId =/= <<>> ->
            case channel_ds:find_by_custom_id(CustomId) of
                Channel when is_map(Channel), map_size(Channel) =:= 0 ->
                    do_create_channel(Uid, Name, Options);
                Channel when is_map(Channel) ->
                    {error, <<"自定义ID已被使用"/utf8>>};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end;
        _ ->
            do_create_channel(Uid, Name, Options)
    end.

do_create_channel(Uid, Name, Opts) ->
    case channel_ds:create_channel(Uid, Name, Opts) of
        {ok, ChannelId} ->
            case channel_ds:find_by_id(ChannelId, ?CHANNEL_SAFE_COLUMNS) of
                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                Channel when is_map(Channel) -> {ok, channel_transfer(Channel)};
                Unexpected -> {error, elib_cnv:safe_to_binary(Unexpected)}
            end;
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% ===================================================================
%% W04.c（channel-group-ux v2.2 §6.4 C1）：带受理回执的频道创建与重放
%% ===================================================================

%% @doc 带受理回执的频道创建（新合同入口，仅当请求携带非空 request_id）。
%% 身份唯一键 (Uid, create_channel, RequestId)。事务序列：advisory 串行化
%% → receipt 占位 → 既有 channel_ds:create_channel_tx（频道+创建者订阅/
%% 管理员+配额，同一 Conn）→ receipt 回填 resource_id/server_accepted_at。
%% 频道创建无初始成员邀请——无通知入队步骤（§6.4 通知义务按效果存在性）。
%% 任何一步失败全部回滚（J23.a）；重放不再创建（J23.b）。7d 服务端年龄
%% 独立决定完整结果（J24.b）；删除例外优先（delete_channel 同事务置位）。
-spec create_channel_with_receipt(
    integer(), binary(), map(), integer(), {binary(), integer()}, binary(), boolean()
) ->
    {ok, map(), accepted | replayed}
    | {error, request_id_conflict | create_not_accepted | binary()}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}
    | {error, {integer(), binary()}}.
create_channel_with_receipt(
    Uid, Name, Opts, MaxChannels, {Scope, WorkspaceId}, RequestId, RecoverOnly
) when
    Scope =:= <<"personal">> orelse Scope =:= <<"workspace">>
->
    case normalize_receipt_name(Name) of
        {error, Reason} ->
            {error, Reason};
        {ok, NameNorm} ->
            Digest = receipt_channel_digest(NameNorm, Opts, Scope, WorkspaceId),
            case RecoverOnly of
                true ->
                    recover_only_channel(Uid, RequestId, Digest);
                false ->
                    case resource_create_receipt_repo:find(Uid, <<"create_channel">>, RequestId) of
                        {ok, Row} ->
                            replay_and_fetch(Uid, Digest, Row);
                        {error, not_found} ->
                            first_accept_channel(
                                Uid,
                                NameNorm,
                                Opts,
                                MaxChannels,
                                Scope,
                                WorkspaceId,
                                RequestId,
                                Digest
                            );
                        {error, Reason} ->
                            ?ERROR_LOG([channel_receipt_find_failed, Uid, RequestId, Reason]),
                            {error, <<"创建受理查询失败，请稍后重试"/utf8>>}
                    end
            end
    end.

%% 首次受理前置（镜像 create_channel/5 的资格顺序：scope 合法性 →
%% workspace 资格），占位/创建/回填同一事务。
-spec first_accept_channel(
    integer(), binary(), map(), integer(), binary(), integer(), binary(), binary()
) ->
    {ok, map(), accepted}
    | {error, binary()}
    | {error, {integer(), binary()}}
    | {error, request_id_conflict}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
first_accept_channel(Uid, NameNorm, Opts, MaxChannels, <<"personal">>, _WsId, RequestId, Digest) ->
    run_first_accept_channel_tx(
        Uid, NameNorm, Opts, MaxChannels, RequestId, Digest
    );
first_accept_channel(
    Uid, NameNorm, Opts, MaxChannels, <<"workspace">>, WorkspaceId, RequestId, Digest
) ->
    case is_integer(WorkspaceId) andalso WorkspaceId > 0 of
        false ->
            {error, {400, <<"scope=workspace 时 workspace_id 必须"/utf8>>}};
        true ->
            case workspace_logic:ensure_can_create_resource(WorkspaceId, Uid) of
                {error, {Code, Msg}} ->
                    {error, {Code, Msg}};
                ok ->
                    Opts2 = Opts#{scope => <<"workspace">>, workspace_id => WorkspaceId},
                    run_first_accept_channel_tx(
                        Uid, NameNorm, Opts2, MaxChannels, RequestId, Digest
                    )
            end
    end.

-spec run_first_accept_channel_tx(
    integer(), binary(), map(), integer(), binary(), binary()
) ->
    {ok, map(), accepted | replayed}
    | {error, binary()}
    | {error, {integer(), binary()}}
    | {error, request_id_conflict}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
run_first_accept_channel_tx(Uid, NameNorm, Opts, MaxChannels, RequestId, Digest) ->
    Options = Opts#{max_channels => MaxChannels},
    case
        elib_pg:with_tx(fun(Conn) ->
            ok = abort_on_receipt_repo_error(
                resource_create_receipt_repo:serialize_tx(
                    Conn, Uid, <<"create_channel">>, RequestId
                )
            ),
            case
                resource_create_receipt_repo:placeholder_tx(
                    Conn, Uid, <<"create_channel">>, RequestId, Digest
                )
            of
                {ok, inserted, _Row} ->
                    case channel_ds:create_channel_tx(Conn, Uid, NameNorm, Options) of
                        {ok, ChannelId} ->
                            accept_receipt_or_abort(
                                resource_create_receipt_repo:accept_tx(
                                    Conn, Uid, <<"create_channel">>, RequestId, ChannelId
                                )
                            ),
                            {ok, ChannelId};
                        {error, Reason} ->
                            throw({abort_tx, Reason})
                    end;
                {ok, existing, Row} ->
                    %% 同键并发前事务已提交：事务内重放，不再创建。
                    replay_channel_receipt(Uid, Digest, Row);
                {error, Reason} ->
                    throw({abort_tx, {receipt_placeholder_failed, Reason}})
            end
        end)
    of
        {ok, ChannelId} ->
            {ok, Channel} = fetch_created_channel(ChannelId),
            {ok, Channel, accepted};
        {ok, ChannelId, replayed} ->
            {ok, Channel} = fetch_created_channel(ChannelId),
            {ok, Channel, replayed};
        {error, {Code, Msg}} when is_integer(Code) ->
            {error, {Code, Msg}};
        {error, channel_creation_limit} ->
            {error, <<"已达频道创建上限"/utf8>>};
        {error, request_id_conflict} = Conflict ->
            Conflict;
        {error, {create_recovery_expired, _}} = Expired ->
            Expired;
        {error, {create_resource_unavailable, _}} = Unavailable ->
            Unavailable;
        {error, Reason} ->
            ?ERROR_LOG([channel_receipt_create_failed, Uid, RequestId, Reason]),
            {error, <<"频道创建失败，请稍后重试"/utf8>>};
        {error, throw, ThrowReason} ->
            ?ERROR_LOG([channel_receipt_create_throw, Uid, RequestId, ThrowReason]),
            {error, <<"频道创建失败，请稍后重试"/utf8>>}
    end.

-spec fetch_created_channel(integer()) -> {ok, map()} | {error, binary()}.
fetch_created_channel(ChannelId) ->
    case channel_ds:find_by_id(ChannelId, ?CHANNEL_SAFE_COLUMNS) of
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Channel when is_map(Channel) ->
            {ok, channel_transfer(Channel)};
        Unexpected ->
            {error, elib_cnv:safe_to_binary(Unexpected)}
    end.

%% 规范化（§6.4）：名称 Unicode 原值仅 trim，空名拒绝；摘要由服务端实际
%% 默认选项（visibility/access_type/join_policy 补 0；description/avatar/
%% custom_id 补空串；tags 去重升序）构成。
-spec normalize_receipt_name(binary()) -> {ok, binary()} | {error, {400, binary()}}.
normalize_receipt_name(Name) when is_binary(Name) ->
    Name2 = string:trim(Name),
    case Name2 of
        <<>> -> {error, {400, <<"频道名称不能为空"/utf8>>}};
        _ -> {ok, Name2}
    end;
normalize_receipt_name(_) ->
    {error, {400, <<"频道名称不能为空"/utf8>>}}.

-spec receipt_channel_digest(binary(), map(), binary(), integer()) -> binary().
receipt_channel_digest(NameNorm, Opts, Scope, WorkspaceId) ->
    resource_create_receipt_repo:canonical_channel_digest(#{
        name => NameNorm,
        scope => Scope,
        workspace_id =>
            case Scope of
                <<"workspace">> -> WorkspaceId;
                <<"personal">> -> 0
            end,
        visibility => elib_cnv:safe_to_integer(maps:get(visibility, Opts, 0)),
        access_type => elib_cnv:safe_to_integer(maps:get(access_type, Opts, 0)),
        join_policy => elib_cnv:safe_to_integer(maps:get(join_policy, Opts, 0)),
        description => elib_cnv:safe_to_binary(maps:get(description, Opts, <<>>)),
        avatar => elib_cnv:safe_to_binary(maps:get(avatar, Opts, <<>>)),
        custom_id =>
            case maps:get(custom_id, Opts, undefined) of
                Bin when is_binary(Bin) -> Bin;
                _ -> <<>>
            end,
        tags =>
            case maps:get(tags, Opts, []) of
                List when is_list(List) -> List;
                _ -> []
            end
    }).

%% 重放判定后取频道出站 DTO：对外形状恒为 {ok, ChannelMap, replayed}
%%（与首受理一致；ChannelId 仅用于错误核对元信息）。
-spec replay_and_fetch(integer(), binary(), map()) ->
    {ok, map(), replayed}
    | {error, request_id_conflict}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
replay_and_fetch(Uid, Digest, Row) ->
    case replay_channel_receipt(Uid, Digest, Row) of
        {ok, ChannelId, replayed} ->
            {ok, Channel} = fetch_created_channel(ChannelId),
            {ok, Channel, replayed};
        Other ->
            Other
    end.

%% recover_only 只查：不新增受理占位/资源（§6.4 恢复状态表）。
-spec recover_only_channel(integer(), binary(), binary()) ->
    {ok, map(), replayed}
    | {error, request_id_conflict | create_not_accepted | binary()}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
recover_only_channel(Uid, RequestId, Digest) ->
    case resource_create_receipt_repo:find(Uid, <<"create_channel">>, RequestId) of
        {ok, Row} ->
            replay_and_fetch(Uid, Digest, Row);
        {error, not_found} ->
            %% 仅表示查询时未见已提交受理，不证明在途请求不会迟到（§6.4）。
            {error, create_not_accepted};
        {error, Reason} ->
            ?ERROR_LOG([channel_receipt_recover_lookup_failed, Uid, RequestId, Reason]),
            {error, <<"创建受理查询失败，请稍后重试"/utf8>>}
    end.

%% 重放判定：先比对规范摘要（同键不同载荷 → 冲突），再按删除/7d/资格
%% 分派（创建者仍是资源管理者；撤权/删除统一不可用，不泄露内容）。
-spec replay_channel_receipt(integer(), binary(), map()) ->
    {ok, integer(), replayed}
    | {error, request_id_conflict}
    | {error, {create_recovery_expired | create_resource_unavailable, integer() | undefined}}.
replay_channel_receipt(Uid, Digest, Row) ->
    case maps:get(<<"request_digest">>, Row) =:= Digest of
        false ->
            {error, request_id_conflict};
        true ->
            case maps:get(<<"resource_id">>, Row) of
                ChannelId when is_integer(ChannelId), ChannelId > 0 ->
                    Expired = maps:get(<<"expired">>, Row) =:= true,
                    Deleted = maps:get(<<"resource_deleted">>, Row) =:= true,
                    replay_channel_decision(Uid, ChannelId, Deleted, Expired);
                _ ->
                    %% 防御：committed 行恒有 resource_id（占位与回填同事务）。
                    {error, {create_resource_unavailable, undefined}}
            end
    end.

-spec replay_channel_decision(integer(), integer(), boolean(), boolean()) ->
    {ok, integer(), replayed}
    | {error, {create_recovery_expired | create_resource_unavailable, integer()}}.
replay_channel_decision(_Uid, ChannelId, true = _Deleted, _Expired) ->
    {error, {create_resource_unavailable, ChannelId}};
replay_channel_decision(_Uid, ChannelId, false, true = _Expired) ->
    {error, {create_recovery_expired, ChannelId}};
replay_channel_decision(Uid, ChannelId, false, false) ->
    case channel_ds:find_by_id(ChannelId, <<"status,creator_uid">>) of
        #{<<"status">> := 1, <<"creator_uid">> := Uid} ->
            {ok, ChannelId, replayed};
        _ ->
            {error, {create_resource_unavailable, ChannelId}}
    end.

-spec abort_on_receipt_repo_error(ok | {error, term()}) -> ok.
abort_on_receipt_repo_error(ok) ->
    ok;
abort_on_receipt_repo_error({error, Reason}) ->
    throw({abort_tx, {receipt_repo_error, Reason}}).

-spec accept_receipt_or_abort({ok, map()} | {error, term()}) -> {ok, map()}.
accept_receipt_or_abort({ok, Row}) ->
    {ok, Row};
accept_receipt_or_abort({error, Reason}) ->
    throw({abort_tx, {receipt_accept_failed, Reason}}).

-spec get_channel(binary(), integer()) -> {ok, map()} | {error, binary() | {integer(), binary()}}.
get_channel(ChannelIdBin, Uid) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_ds:find_by_id_with_price(ChannelId) of
                {error, _} ->
                    {error, <<"频道不存在"/utf8>>};
                Channel when is_map(Channel), map_size(Channel) =:= 0 -> {error, <<"频道不存在"/utf8>>};
                Channel when is_map(Channel) ->
                    channel_view(Channel, ChannelId, Uid);
                _Unexpected ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec get_channel_by_custom_id(binary(), integer()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
get_channel_by_custom_id(CustomId, Uid) ->
    case channel_ds:find_by_custom_id(CustomId) of
        {error, _} ->
            {error, <<"频道不存在"/utf8>>};
        Channel when is_map(Channel), map_size(Channel) =:= 0 ->
            {error, <<"频道不存在"/utf8>>};
        Channel when is_map(Channel) ->
            ChannelId = maps:get(<<"id">>, Channel, 0),
            case is_integer(ChannelId) andalso ChannelId > 0 of
                false ->
                    {error, <<"频道不存在"/utf8>>};
                true ->
                    case channel_with_price_for_custom_id(Channel, ChannelId) of
                        {error, _} ->
                            {error, <<"频道不存在"/utf8>>};
                        ChannelWithPrice when is_map(ChannelWithPrice) ->
                            channel_view(ChannelWithPrice, ChannelId, Uid);
                        _Unexpected ->
                            {error, <<"频道不存在"/utf8>>}
                    end
            end;
        _Unexpected ->
            {error, <<"频道不存在"/utf8>>}
    end.

%% Scope comes from the repository projection; legacy personal projections omit it.
channel_view(Channel, ChannelId, Uid) ->
    Access =
        case maps:get(<<"scope">>, Channel, <<"personal">>) of
            <<"workspace">> -> workspace_resolver:ensure_channel_member_access(Uid, ChannelId);
            _ -> ok
        end,
    case Access of
        ok ->
            case add_user_channel_state(Channel, ChannelId, Uid) of
                {error, _} = Error -> Error;
                ChannelState when is_map(ChannelState) -> {ok, channel_transfer(ChannelState)}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 详情接口返回与当前用户有关的频道状态；付费频道的内容权益只认购买订单。
-spec add_user_channel_state(map(), integer(), integer()) -> map() | {error, binary()}.
add_user_channel_state(Channel, ChannelId, Uid) ->
    case has_purchased(Channel, ChannelId, Uid) of
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        HasPurchased when is_boolean(HasPurchased) ->
            UserRole = channel_logic_common:get_user_role(ChannelId, Uid),
            IsSubscribed =
                case UserRole of
                    % 创建者恒订阅
                    3 -> true;
                    _ -> channel_subscription_ds:is_subscribed(ChannelId, Uid)
                end,
            Channel#{
                user_role => UserRole,
                is_subscribed => IsSubscribed,
                has_purchased => HasPurchased
            }
    end.

-spec has_purchased(map(), integer(), integer()) -> boolean() | {error, term()}.
has_purchased(Channel, ChannelId, Uid) ->
    case maps:get(<<"access_type">>, Channel, 0) of
        1 -> channel_order_ds:has_purchased(ChannelId, Uid);
        _ -> false
    end.

-spec channel_with_price_for_custom_id(map(), integer()) -> map() | {error, term()}.
channel_with_price_for_custom_id(Channel, ChannelId) ->
    case maps:get(<<"access_type">>, Channel, 0) of
        1 ->
            case channel_ds:find_by_id_with_price(ChannelId) of
                Row when is_map(Row) -> Row;
                {error, Reason} -> {error, Reason};
                _ -> {error, not_found}
            end;
        _ ->
            Channel
    end.

-spec update_channel(integer(), binary(), map()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
update_channel(Uid, ChannelIdBin, Data) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            Role = channel_logic_common:get_user_role(ChannelId, Uid),
            case Role >= 2 of
                false ->
                    {error, <<"无权限操作"/utf8>>};
                true ->
                    AllowedFields = [
                        <<"name">>, <<"description">>, <<"avatar">>, <<"tags">>, <<"custom_id">>
                    ],
                    FilteredData = maps:filter(
                        fun(K, _) -> lists:member(K, AllowedFields) end, Data
                    ),
                    case resolve_custom_id_update(ChannelId, FilteredData) of
                        {ok, Data2} ->
                            do_update_channel(ChannelId, Data2);
                        {error, _} = Err ->
                            Err
                    end
            end
    end.

%% @doc update 路径的 custom_id 二次校验（对齐客户端注释中的 resolve_custom_id_update 语义）：
%% 设过即锁定——仅当频道当前 custom_id 为空（未设过）时允许补设，已设过则忽略本次传入；
%% 校验规则与 create_channel 一致（仅接受非空 binary + 唯一性检查），
%% 对显式传入的非法值返回明确错误，避免静默过滤（BUG#134）。
-spec resolve_custom_id_update(integer(), map()) -> {ok, map()} | {error, binary()}.
resolve_custom_id_update(ChannelId, Data) ->
    case maps:find(<<"custom_id">>, Data) of
        error ->
            {ok, Data};
        {ok, CustomId} when is_binary(CustomId), CustomId =/= <<>> ->
            case channel_ds:find_by_id(ChannelId, <<"custom_id">>) of
                Channel when is_map(Channel) ->
                    case maps:get(<<"custom_id">>, Channel, <<>>) of
                        Current when is_binary(Current), Current =/= <<>> ->
                            % 已设过即锁定：忽略本次传入，不覆盖
                            {ok, maps:remove(<<"custom_id">>, Data)};
                        _ ->
                            % 无行返回空 map #{} = 未占用放行；非空 map = 已占用；
                            % {error, _}（SQL 出错）fail-open 放行，DB 唯一约束
                            % uk_channel_custom_id 兜底并发竞态
                            case channel_ds:find_by_custom_id(CustomId) of
                                Found when is_map(Found), map_size(Found) =:= 0 ->
                                    {ok, Data};
                                Found when is_map(Found) ->
                                    {error, <<"自定义ID已被使用"/utf8>>};
                                {error, _} ->
                                    {ok, Data}
                            end
                    end;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end;
        {ok, _Invalid} ->
            {error, <<"自定义ID格式无效"/utf8>>}
    end.

-spec do_update_channel(integer(), map()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
do_update_channel(ChannelId, Data) ->
    case channel_ds:update(ChannelId, Data#{updated_at => elib_dt:now()}) of
        {ok, _} ->
            case channel_ds:find_by_id(ChannelId, ?CHANNEL_SAFE_COLUMNS) of
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)};
                Channel when is_map(Channel) ->
                    channel_logic_notify:notify_channel_update(ChannelId, Channel),
                    {ok, channel_transfer(Channel)};
                Unexpected ->
                    {error, elib_cnv:safe_to_binary(Unexpected)}
            end;
        %% T7 收口：归档守卫（980）等稳定错误码透传
        {error, {Code, Msg}} when is_integer(Code) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec delete_channel(integer(), binary()) -> ok | {error, binary() | {integer(), binary()}}.
delete_channel(Uid, ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            Role = channel_logic_common:get_user_role(ChannelId, Uid),
            case Role == 3 of
                false ->
                    {error, <<"只有创建者可以删除频道"/utf8>>};
                true ->
                    SubscriberUids = safe_subscriber_uids(ChannelId),
                    case channel_ds:delete(ChannelId) of
                        {ok, _} ->
                            channel_logic_notify:notify_channel_deleted(ChannelId, SubscriberUids),
                            ok;
                        %% T7 收口：归档守卫（980）等稳定错误码透传
                        {error, {Code, Msg}} when is_integer(Code) ->
                            {error, {Code, Msg}};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end
            end
    end.

%% ===================================================================
%% GZAPP-02/G4：频道归档/恢复（复用 status 软删列语义：归档=1→0、恢复=0→1，
%% 与删除（→-1）区分；无 DDL。所有列表/发现/搜索查询恒过滤 status=1，
%% 归档频道自然退出各公开面；workspace_guard 同事务（ws 频道所属工作区
%% 已归档时稳定 980 拒绝）。权限与 delete 同口径：仅创建者（Role=3）。
%% ===================================================================

-spec archive_channel(integer(), binary()) ->
    {ok, map()} | {error, binary()} | {error, {integer(), binary()}}.
archive_channel(Uid, ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            %% R3-5：授权判定在 channel_ds 的**写事务内**执行（判定与归档同一
            %% Conn/快照），不再先判定后写入。
            case
                channel_ds:archive_with_authz(ChannelId, fun(Conn) ->
                    manage_authority_tx(Conn, Uid, ChannelId, <<"只有创建者可以归档频道"/utf8>>)
                end)
            of
                {ok, 1} ->
                    _ = ?INFO_LOG([channel_archived, ChannelId, Uid]),
                    notify_channel_status_changed(ChannelId),
                    {ok, #{
                        channel_id => ChannelId,
                        status => <<"archived"/utf8>>
                    }};
                {ok, 0} ->
                    {error, {409, <<"频道已处于归档或删除状态"/utf8>>}};
                %% 事务内授权拒绝：映射回既有对外形状（裸 message，与原
                %% 事务外判定完全一致）
                {error, {channel_authz_denied, AuthzMsg}} ->
                    {error, AuthzMsg};
                %% T7 收口：归档守卫（980）等稳定错误码透传
                {error, {Code, Msg}} when is_integer(Code) ->
                    {error, {Code, Msg}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([channel_archive_failed, ChannelId, Uid, Reason]),
                    {error, <<"归档失败，请稍后重试"/utf8>>}
            end
    end.

-spec restore_channel(integer(), binary()) ->
    {ok, map()} | {error, binary()} | {error, {integer(), binary()}}.
restore_channel(Uid, ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case
                channel_ds:restore_with_authz(ChannelId, fun(Conn) ->
                    manage_authority_tx(Conn, Uid, ChannelId, <<"只有创建者可以恢复频道"/utf8>>)
                end)
            of
                {ok, 1} ->
                    _ = ?INFO_LOG([channel_restored, ChannelId, Uid]),
                    notify_channel_status_changed(ChannelId),
                    {ok, #{
                        channel_id => ChannelId,
                        status => <<"active"/utf8>>
                    }};
                {ok, 0} ->
                    %% active（未归档）或 -1（已删除，不可恢复）
                    {error, {409, <<"频道不处于可恢复的归档状态"/utf8>>}};
                %% 事务内授权拒绝 → 既有对外形状（裸 message）
                {error, {channel_authz_denied, AuthzMsg}} ->
                    {error, AuthzMsg};
                {error, {Code, Msg}} when is_integer(Code) ->
                    {error, {Code, Msg}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([channel_restore_failed, ChannelId, Uid, Reason]),
                    {error, <<"恢复失败，请稍后重试"/utf8>>}
            end
    end.

%% GZAPP-03/D04：频道归档/恢复的双授权源——频道创建者（角色 3）或本频道
%% 所属 Workspace 的 Organization owner/admin。两者都不满足时返回该操作
%% 的既有拒绝文案（不区分「非创建者」与「非 org 管理者」，不泄露存在性）；
%% 授权链 DB 异常 fail-closed 503 原样透传。
-spec manage_authority(integer(), integer(), binary()) -> ok | {error, binary()}.
manage_authority(Uid, ChannelId, ForbiddenMsg) ->
    case channel_logic_common:get_user_role(ChannelId, Uid) == 3 of
        true ->
            ok;
        false ->
            case organization_resource_authority:ensure_manager({channel, ChannelId}, Uid) of
                ok -> ok;
                {error, {403, _Msg}} -> {error, ForbiddenMsg};
                {error, {503, Msg}} -> {error, Msg}
            end
    end.

%% @doc 事务内授权判定（R3-5）：与 manage_authority/3 同口径、同对外形状
%% （ok | {error, binary()}），但两次判定都在传入的 Conn 上完成——调用方在
%% 写事务内调用它，判定依据即本事务快照。
-spec manage_authority_tx(any(), integer(), integer(), binary()) -> ok | {error, binary()}.
manage_authority_tx(Conn, Uid, ChannelId, ForbiddenMsg) ->
    case channel_logic_common:get_user_role_tx(Conn, ChannelId, Uid) == 3 of
        true ->
            ok;
        false ->
            case
                organization_resource_authority:ensure_manager_tx(Conn, {channel, ChannelId}, Uid)
            of
                ok -> ok;
                {error, {403, _Msg}} -> {error, ForbiddenMsg};
                {error, {503, Msg}} -> {error, Msg}
            end
    end.

%% 归档/恢复后向订阅者广播频道更新（复用 channel_updated 通道，
%% 客户端据 channel.status 刷新可见性）。
-spec notify_channel_status_changed(integer()) -> ok.
notify_channel_status_changed(ChannelId) ->
    case channel_ds:find_by_id(ChannelId, ?CHANNEL_SAFE_COLUMNS) of
        Channel when is_map(Channel), map_size(Channel) > 0 ->
            channel_logic_notify:notify_channel_update(ChannelId, channel_transfer(Channel));
        _ ->
            ok
    end.

-spec publish_message(integer(), binary(), binary(), binary(), map()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
publish_message(Uid, ChannelIdBin, Content, MsgType, Payload) ->
    publish_with_tags(Uid, ChannelIdBin, Content, MsgType, Payload, legacy).

-spec publish_message(integer(), binary(), binary(), binary(), map(), binary()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
publish_message(Uid, ChannelIdBin, Content, MsgType, Payload, RequestId) ->
    publish_with_tags(
        Uid, ChannelIdBin, Content, MsgType, Payload, {request_id, RequestId}
    ).

%% Validate the already decoded map before any role, moderation, persistence or notify.
publish_with_tags(Uid, ChannelId, Content, MsgType, Payload, StoreMode) ->
    case channel_message_tags:normalize_payload(Payload) of
        {ok, Normalized} ->
            do_publish_message(Uid, ChannelId, Content, MsgType, Normalized, StoreMode);
        {error, _} ->
            {error, <<"消息标签或载荷格式无效"/utf8>>}
    end.

do_publish_message(Uid, ChannelIdBin, Content, MsgType, Payload, StoreMode) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            Role = channel_logic_common:get_user_role(ChannelId, Uid),
            case Role < 1 of
                true ->
                    {error, <<"只有管理员可以发布消息"/utf8>>};
                false ->
                    %% 帖子图片归属硬校验（prod 2026-10-20 事故二加固）：
                    %% 图片只允许引用本频道的 channel 附件或 public 资源；
                    %% private/跨频道附件在发布时拒绝——否则发布成功但订阅者
                    %% 渲染 view_url 400「无权访问该附件」，失败被推迟到
                    %% 每个订阅者的渲染期才暴露。
                    case validate_post_images(ChannelId, Payload) of
                        ok ->
                            do_publish_after_images(
                                Uid, ChannelId, Content, MsgType, Payload, StoreMode
                            );
                        {error, _} = ImgErr ->
                            ImgErr
                    end
            end
    end.

do_publish_after_images(Uid, ChannelId, Content, MsgType, Payload, StoreMode) ->
    %% R-03 审核门：high 命中词在落库前直接拦截；
    %% medium/low 命中先发后审（入队复核），入队失败 fail-open。
    case moderation_business:inspect_channel(ChannelId, MsgType, Content, Payload) of
        {blocked, _Hits} ->
            {error, <<"内容包含违规词汇，发布失败"/utf8>>};
        {error, Reason} ->
            {error, Reason};
        Verdict ->
            case
                channel_message_store:publish(StoreMode, ChannelId, Uid, Content, MsgType, Payload)
            of
                {ok, MessageId, Status} ->
                    case Status of
                        inserted ->
                            maybe_enqueue_review(
                                moderation_business:bind_version(Verdict, <<>>),
                                MessageId,
                                ChannelId,
                                Uid,
                                Content
                            );
                        duplicate ->
                            ok
                    end,
                    case channel_message_ds:find_by_id(MessageId) of
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)};
                        Message when is_map(Message) ->
                            Message2 = message_transfer(Message),
                            maybe_notify_new_message(ChannelId, Message2, Status),
                            {ok, Message2}
                    end;
                %% T7 归档写守卫稳定错误码（980）：元组形态透传给
                %% handler 映射 envelope code，不做 binary 折叠
                {error, {Code, Msg}} when is_integer(Code) ->
                    {error, {Code, Msg}};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

%% @doc 帖子图片归属校验：images 中的附件 key 必须是本频道的 channel 附件
%% 或 public 资源。非附件形态（外链/presigned URL）与无 uri 的旧形态跳过；
%% 附件归属查询失败 fail-closed（不发布归属不确定的图片）。
validate_post_images(ChannelId, Payload) ->
    case maps:get(<<"images">>, Payload, undefined) of
        Images when is_list(Images) ->
            validate_post_images_list(Images, ChannelId, 1);
        _ ->
            ok
    end.

validate_post_images_list([], _ChannelId, _Index) ->
    ok;
validate_post_images_list([#{<<"uri">> := Uri} | Rest], ChannelId, Index) when
    is_binary(Uri), Uri =/= <<>>
->
    case elib_oss:owner_of_key(Uri) of
        {ok, _OwnerUid} ->
            case validate_post_image_scope(Uri, ChannelId) of
                ok -> validate_post_images_list(Rest, ChannelId, Index + 1);
                {error, Reason} -> {error, image_error_message(Index, Reason)}
            end;
        {error, invalid_key} ->
            %% 外链/presigned URL 等非本系统附件 key：不校验归属
            validate_post_images_list(Rest, ChannelId, Index + 1)
    end;
validate_post_images_list([_Other | Rest], ChannelId, Index) ->
    %% 无 uri 字段的旧形态项：交由既有渲染兼容处理
    validate_post_images_list(Rest, ChannelId, Index).

validate_post_image_scope(ObjectKey, ChannelId) ->
    case attachment_ds:find_by_path(ObjectKey) of
        {ok, Rec} ->
            case maps:get(<<"scope">>, Rec, undefined) of
                <<"public">> ->
                    ok;
                <<"channel">> ->
                    case elib_cnv:safe_to_integer(maps:get(<<"scope_ref">>, Rec, undefined)) of
                        ChannelId -> ok;
                        _ -> {error, scope_mismatch}
                    end;
                _ ->
                    {error, scope_mismatch}
            end;
        {error, not_found} ->
            {error, not_found};
        {error, _Other} ->
            %% fail-closed：归属查询故障时不放行发布
            {error, lookup_failed}
    end.

image_error_message(Index, Reason) ->
    Hint =
        case Reason of
            scope_mismatch ->
                <<"不是本频道的附件或公开资源，请在本频道重新上传后再发布"/utf8>>;
            not_found ->
                <<"附件不存在，请重新上传后再发布"/utf8>>;
            lookup_failed ->
                <<"归属校验暂不可用，请稍后重试"/utf8>>
        end,
    <<"第"/utf8, (integer_to_binary(Index))/binary, "张图片"/utf8, Hint/binary>>.

%% R-03：queued 命中写人工复核队列；入队失败仅记日志不阻断发布（fail-open）。
maybe_enqueue_review({queued, Hits}, MessageId, ChannelId, Uid, Content) ->
    _ = moderation_policy:enqueue(channel_message, MessageId, ChannelId, Uid, <<>>, Content, Hits),
    ok;
maybe_enqueue_review(_Other, _MessageId, _ChannelId, _Uid, _Content) ->
    ok.

maybe_notify_new_message(ChannelId, Message, inserted) ->
    channel_logic_notify:broadcast_channel_message(ChannelId, Message),
    push_unread_updates(ChannelId);
maybe_notify_new_message(_ChannelId, _Message, duplicate) ->
    ok.

%% 单篇读取仍先检查频道内容权益，再验证消息属于同一频道。
-spec get_message(integer(), binary(), binary()) -> {ok, map()} | {error, term()}.
get_message(Uid, ChannelIdBin, MessageIdBin) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    MessageId = channel_logic_common:decode_positive_id(MessageIdBin),
    case ChannelId > 0 andalso MessageId > 0 of
        false ->
            {error, <<"内容不可用"/utf8>>};
        true ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok -> read_message(Uid, ChannelId, MessageId);
                Error -> Error
            end
    end.

read_message(Uid, ChannelId, MessageId) ->
    case channel_message_ds:find_by_id(MessageId) of
        #{<<"channel_id">> := ChannelId, <<"revoked">> := true} ->
            {error, <<"内容不可用"/utf8>>};
        #{<<"channel_id">> := ChannelId} = Message ->
            [Result] = attach_my_reactions(Uid, [message_transfer(Message)]),
            {ok, Result};
        {error, _} = Error ->
            Error;
        _ ->
            {error, <<"内容不可用"/utf8>>}
    end.

-spec get_messages(integer(), binary(), integer(), integer()) ->
    {ok, list(map())} | {error, binary()}.
get_messages(Uid, ChannelIdBin, Cursor, Limit) ->
    get_messages_with_access(Uid, ChannelIdBin, Cursor, Limit).

get_messages_with_access(Uid, ChannelIdBin, Cursor, Limit) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    case message_rows(ChannelId, Cursor, Limit) of
                        {ok, Messages} when is_list(Messages) ->
                            message_page(Uid, Cursor, Messages, Limit);
                        {ok, UnexpectedMessages} ->
                            {error, elib_cnv:safe_to_binary(UnexpectedMessages)};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)};
                        Unexpected ->
                            {error, elib_cnv:safe_to_binary(Unexpected)}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Both APIs enter the existing access guard before any cursor-backed read.
get_messages_page(Uid, ChannelIdBin, Cursor, Limit) when
    is_integer(Limit), Limit >= 1, Limit =< 200
->
    get_messages_with_access(Uid, ChannelIdBin, {page, Cursor}, Limit);
get_messages_page(_, _, _, _) ->
    {error, <<"invalid_message_limit">>}.

message_rows(ChannelId, {page, Raw}, Limit) ->
    case message_cursor(Raw) of
        {ok, Cursor} -> channel_message_ds:list_by_channel_page(ChannelId, Cursor, Limit);
        {error, _} = Error -> Error
    end;
message_rows(ChannelId, Cursor, Limit) ->
    channel_message_ds:list_by_channel(ChannelId, Cursor, Limit).

message_cursor(<<>>) ->
    {ok, 0};
message_cursor(Ms) when is_integer(Ms), Ms >= 0 -> legacy_message_cursor(Ms);
message_cursor(Raw) when is_binary(Raw), byte_size(Raw) =< 512 ->
    case re:run(Raw, <<"\\A[0-9]+\\z">>, [{capture, none}]) of
        match ->
            legacy_message_cursor(binary_to_integer(Raw));
        nomatch ->
            case msg_offline_cursor:decode(Raw) of
                {ok, {At, Id}} -> positive_cursor_id(At, Id);
                _ -> {error, <<"invalid_message_cursor">>}
            end
    end;
message_cursor(_) ->
    {error, <<"invalid_message_cursor">>}.

%% Preserve integer-value digit inference, including zero-padded strings.
%% Guard the real helper's 2100 cap before it can silently return the epoch.
legacy_message_cursor(0) ->
    {ok, 0};
legacy_message_cursor(Value) when is_integer(Value), Value > 0 ->
    Digits = byte_size(integer_to_binary(Value)),
    Factor =
        case Digits of
            N when N =< 10 -> 1;
            N when N =< 13 -> 1000;
            N when N =< 16 -> 1000000;
            _ -> 1000000000
        end,
    case Value =< 4102444800 * Factor of
        true ->
            try
                {ok, {legacy, elib_dt:to_rfc3339(Value)}}
            catch
                _:_ -> {error, <<"invalid_message_cursor">>}
            end;
        false ->
            {error, <<"invalid_message_cursor">>}
    end;
legacy_message_cursor(_) ->
    {error, <<"invalid_message_cursor">>}.

positive_cursor_id(At, Id) ->
    case re:run(Id, <<"\\A[1-9][0-9]{0,18}\\z">>, [{capture, none}]) of
        match ->
            case binary_to_integer(Id) =< 9223372036854775807 of
                true -> {ok, {At, Id}};
                false -> {error, <<"invalid_message_cursor">>}
            end;
        nomatch ->
            {error, <<"invalid_message_cursor">>}
    end.

message_page(Uid, {page, _}, Rows, Limit) ->
    PageRows = lists:sublist(Rows, Limit),
    Next =
        case length(Rows) > Limit of
            true ->
                Last = lists:last(PageRows),
                msg_offline_cursor:encode({
                    maps:get(<<"cursor_at">>, Last), integer_to_binary(maps:get(<<"id">>, Last))
                });
            false ->
                null
        end,
    Messages = [message_transfer(maps:remove(<<"cursor_at">>, M)) || M <- PageRows],
    {ok, #{list => attach_my_reactions(Uid, Messages), next_cursor => Next}};
message_page(Uid, _, Rows, _) ->
    {ok, attach_my_reactions(Uid, [message_transfer(M) || M <- Rows, is_map(M)])}.

-spec mark_as_read(integer(), binary(), binary()) -> ok | {error, binary()}.
mark_as_read(Uid, ChannelIdBin, _MessageIdBin) ->
    %% W08（§6.8）：本端点保持旧语义——清零聚合 unread_count；
    %% **不推进**新版权里 view_seq（旧 clear_unread 不构成精确查看水位，
    %% 精确边界走 mark_viewed/3）。
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    %% 派生已读写由 DS 在事务内锁 Workspace；归档时跳过写入，
                    %% 读取仍返回成功。
                    case channel_subscription_ds:clear_unread(ChannelId, Uid) of
                        {ok, _} ->
                            channel_logic_notify:notify_channel_unread_count(ChannelId, Uid, 0),
                            ok;
                        skipped ->
                            ok;
                        {error, ClearReason} ->
                            ?ERROR_LOG([
                                "channel_clear_unread_failed", ChannelId, Uid, ClearReason
                            ]),
                            ok
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc W08（§6.8 C4/D3）：显式"标已查看至此更新"。
%% boundary 为客户端本次成功内容响应持有的更新边界（§6.8：不从后来未
%% 展示的新推送取最大值）；服务端 GREATEST 推进（晚到旧边界不倒退）并
%% 夹到当前发布上界。返回新水位与未查看更新数（当前可读、未撤销且
%% update_seq > V 的发布项；作者自己的新发布已在发布事务内推进其 V）。
-spec mark_viewed(integer(), binary(), binary()) ->
    {ok, map()} | skipped | {error, binary()}.
mark_viewed(Uid, ChannelIdBin, BoundaryBin) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case viewed_boundary(BoundaryBin) of
                {ok, Boundary} ->
                    case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                        ok ->
                            mark_viewed_after_access(Uid, ChannelId, Boundary);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Msg} ->
                    {error, Msg}
            end
    end.

mark_viewed_after_access(Uid, ChannelId, Boundary) ->
    case channel_subscription_repo:advance_view(ChannelId, Uid, Boundary) of
        {ok, ViewSeq} ->
            case channel_subscription_repo:count_unseen(ChannelId, ViewSeq) of
                {ok, Unseen} ->
                    {ok, #{view_seq => ViewSeq, unseen_count => Unseen}};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end;
        skipped ->
            skipped;
        {error, not_subscribed} ->
            {error, <<"未订阅该频道"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec viewed_boundary(binary()) -> {ok, non_neg_integer()} | {error, binary()}.
viewed_boundary(Bin) when is_binary(Bin) ->
    case catch binary_to_integer(Bin) of
        N when is_integer(N), N >= 0 -> {ok, N};
        _ -> {error, <<"查看边界无效"/utf8>>}
    end;
viewed_boundary(_) ->
    {error, <<"查看边界无效"/utf8>>}.

-spec search_channels(binary(), integer()) -> {ok, list(map())} | {error, binary()}.
search_channels(Keyword, Limit) ->
    case channel_ds:search(Keyword, Limit, ?CHANNEL_SAFE_COLUMNS) of
        {ok, Channels} when is_list(Channels) ->
            {ok, [channel_transfer(C) || C <- Channels, is_map(C)]};
        {ok, UnexpectedChannels} ->
            {error, elib_cnv:safe_to_binary(UnexpectedChannels)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Unexpected ->
            {error, elib_cnv:safe_to_binary(Unexpected)}
    end.

-spec get_discover_channels(integer()) -> {ok, list(map())} | {error, binary()}.
get_discover_channels(Limit) ->
    case channel_ds:list_discover(Limit, ?CHANNEL_SAFE_COLUMNS) of
        {ok, Channels} when is_list(Channels) ->
            {ok, [channel_transfer(C) || C <- Channels, is_map(C)]};
        {ok, UnexpectedChannels} ->
            {error, elib_cnv:safe_to_binary(UnexpectedChannels)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        Unexpected ->
            {error, elib_cnv:safe_to_binary(Unexpected)}
    end.

-spec add_admin(integer(), binary(), integer(), integer()) ->
    ok | {error, binary() | {integer(), binary()}}.
add_admin(Uid, ChannelIdBin, NewAdminUid, Role) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            CurrentRole = channel_logic_common:get_user_role(ChannelId, Uid),
            case CurrentRole == 3 of
                false ->
                    {error, <<"只有创建者可以添加管理员"/utf8>>};
                true ->
                    %% T7 归档写守卫（R3 #13 收口）：守卫已下沉 channel_admin_ds
                    %% 写事务（FOR UPDATE 同事务），此处不再前置检查。
                    do_add_admin(ChannelId, NewAdminUid, Role)
            end
    end.

do_add_admin(ChannelId, NewAdminUid, Role) ->
    Now = elib_dt:now(),
    Data = #{
        channel_id => ChannelId,
        user_id => NewAdminUid,
        role => Role,
        created_at => Now
    },
    case channel_admin_ds:add(Data) of
        {ok, _} ->
            %% ENT-BE-01：治理成功路径 best-effort 通知新管理员（失败仅记日志）
            channel_logic_notify:notify_admin_added(ChannelId, NewAdminUid, Role),
            ok;
        %% 稳定错误码（980 等）原样透传供 handler envelope 映射
        {error, {Code, Msg}} when is_integer(Code) -> {error, {Code, Msg}};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec remove_admin(integer(), binary(), integer()) ->
    ok | {error, binary() | {integer(), binary()}}.
remove_admin(Uid, ChannelIdBin, AdminUid) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            CurrentRole = channel_logic_common:get_user_role(ChannelId, Uid),
            case CurrentRole == 3 of
                false ->
                    {error, <<"只有创建者可以移除管理员"/utf8>>};
                true ->
                    %% T7 归档写守卫（R3 #13 收口）：守卫已下沉 channel_admin_ds 写事务
                    case channel_admin_ds:delete(ChannelId, AdminUid) of
                        {ok, _} ->
                            %% ENT-BE-01：治理成功路径 best-effort 通知被移除管理员
                            channel_logic_notify:notify_admin_removed(ChannelId, AdminUid),
                            ok;
                        {error, {Code, Msg}} when is_integer(Code) -> {error, {Code, Msg}};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end
            end
    end.

-spec pin_message(integer(), binary(), boolean()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
pin_message(Uid, MessageIdBin, IsPinned) ->
    MessageId = decode_positive_id(MessageIdBin),
    case MessageId of
        0 ->
            {error, <<"消息不存在"/utf8>>};
        _ ->
            case channel_message_ds:find_by_id(MessageId) of
                {error, _} ->
                    {error, <<"消息不存在"/utf8>>};
                Message when is_map(Message) ->
                    ChannelId = maps:get(<<"channel_id">>, Message, 0),
                    case is_integer(ChannelId) andalso ChannelId > 0 of
                        true ->
                            Role = channel_logic_common:get_user_role(ChannelId, Uid),
                            case Role < 2 of
                                true ->
                                    {error, <<"只有管理员可以置顶消息"/utf8>>};
                                false ->
                                    Now = elib_dt:now(),
                                    case
                                        channel_message_ds:update(MessageId, #{
                                            is_pinned => IsPinned,
                                            updated_at => Now
                                        })
                                    of
                                        {ok, _} ->
                                            case channel_message_ds:find_by_id(MessageId) of
                                                {error, Reason} ->
                                                    {error, elib_cnv:safe_to_binary(Reason)};
                                                Message2 when is_map(Message2) ->
                                                    {ok, message_transfer(Message2)}
                                            end;
                                        %% T7 收口：归档守卫（980）等稳定错误码透传
                                        {error, {Code, Msg}} when is_integer(Code) ->
                                            {error, {Code, Msg}};
                                        {error, Reason} ->
                                            {error, elib_cnv:safe_to_binary(Reason)}
                                    end
                            end;
                        false ->
                            {error, <<"消息不存在"/utf8>>}
                    end;
                _UnexpectedMessage ->
                    {error, <<"消息不存在"/utf8>>}
            end
    end.

-spec delete_message(integer(), binary()) -> ok | {error, binary() | {integer(), binary()}}.
delete_message(Uid, MessageIdBin) ->
    MessageId = decode_positive_id(MessageIdBin),
    case MessageId of
        0 ->
            {error, <<"消息不存在"/utf8>>};
        _ ->
            case channel_message_ds:find_by_id(MessageId) of
                {error, _} ->
                    {error, <<"消息不存在"/utf8>>};
                Message when is_map(Message) ->
                    ChannelId = maps:get(<<"channel_id">>, Message, 0),
                    AuthorId = maps:get(<<"author_id">>, Message, 0),
                    case
                        is_integer(ChannelId) andalso
                            ChannelId > 0 andalso
                            is_integer(AuthorId) andalso
                            AuthorId > 0
                    of
                        false ->
                            {error, <<"消息不存在"/utf8>>};
                        true ->
                            Role = channel_logic_common:get_user_role(ChannelId, Uid),
                            case Role >= 2 orelse AuthorId == Uid of
                                false ->
                                    {error, <<"无权限删除此消息"/utf8>>};
                                true ->
                                    case channel_message_ds:delete(MessageId) of
                                        {ok, _} ->
                                            channel_logic_notify:notify_message_deleted(
                                                ChannelId, MessageId
                                            ),
                                            ok;
                                        %% T7 收口：归档守卫（980）等稳定错误码透传
                                        {error, {Code, Msg}} when is_integer(Code) ->
                                            {error, {Code, Msg}};
                                        {error, Reason} ->
                                            {error, elib_cnv:safe_to_binary(Reason)}
                                    end
                            end
                    end;
                _UnexpectedMessage ->
                    {error, <<"消息不存在"/utf8>>}
            end
    end.

-spec revoke_message(integer(), binary(), binary()) ->
    ok | {error, binary() | {integer(), binary()}}.
revoke_message(Uid, ChannelIdBin, MessageIdBin) ->
    StartMs = elib_dt:millisecond(),
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    MessageId = decode_positive_id(MessageIdBin),
    Result =
        case ChannelId of
            0 ->
                {error, <<"频道不存在"/utf8>>};
            _ when MessageId =:= 0 ->
                {error, <<"消息不存在"/utf8>>};
            _ ->
                case channel_message_ds:find_by_id(MessageId) of
                    {error, _} ->
                        {error, <<"消息不存在"/utf8>>};
                    Message when is_map(Message) ->
                        MessageChannelId = maps:get(<<"channel_id">>, Message, 0),
                        MessageAuthorId = maps:get(<<"author_id">>, Message, 0),
                        case
                            is_integer(MessageChannelId) andalso
                                MessageChannelId > 0 andalso
                                is_integer(MessageAuthorId) andalso
                                MessageAuthorId > 0
                        of
                            false ->
                                {error, <<"消息不存在"/utf8>>};
                            true ->
                                case MessageChannelId =:= ChannelId of
                                    false ->
                                        {error, <<"消息不存在"/utf8>>};
                                    true ->
                                        Role = channel_logic_common:get_user_role(ChannelId, Uid),
                                        case Role >= 2 orelse MessageAuthorId =:= Uid of
                                            false ->
                                                {error, <<"无权限撤回此消息"/utf8>>};
                                            true ->
                                                Revoked = maps:get(<<"revoked">>, Message, false),
                                                case Revoked =:= true of
                                                    true ->
                                                        ok;
                                                    false ->
                                                        case is_within_revoke_window(Message) of
                                                            false ->
                                                                {error, <<"撤回时间已超出限制"/utf8>>};
                                                            true ->
                                                                RevokedAt = elib_dt:now(),
                                                                case
                                                                    channel_message_ds:revoke(
                                                                        MessageId, Uid, RevokedAt
                                                                    )
                                                                of
                                                                    {ok, _} ->
                                                                        channel_logic_notify:notify_message_revoked(
                                                                            ChannelId,
                                                                            MessageId,
                                                                            Uid,
                                                                            RevokedAt
                                                                        ),
                                                                        ok;
                                                                    %% T7 收口：归档守卫（980）等稳定错误码透传
                                                                    {error, {Code, Msg}} when
                                                                        is_integer(Code)
                                                                    ->
                                                                        {error, {Code, Msg}};
                                                                    {error, Reason} ->
                                                                        {error,
                                                                            elib_cnv:safe_to_binary(
                                                                                Reason
                                                                            )};
                                                                    UnexpectedRevoke ->
                                                                        {error,
                                                                            elib_cnv:safe_to_binary(
                                                                                UnexpectedRevoke
                                                                            )}
                                                                end
                                                        end
                                                end
                                        end
                                end
                        end;
                    _UnexpectedMessage ->
                        {error, <<"消息不存在"/utf8>>}
                end
        end,
    channel_logic_common:log_channel_action(
        Uid, ChannelId, MessageId, <<"revoke_message">>, Result, StartMs
    ),
    Result.

%% @doc 编辑频道消息（仅作者本人，受编辑时间窗约束，已撤回消息不可编辑）
-spec edit_message(integer(), binary(), binary(), binary()) ->
    ok | {error, binary() | {integer(), binary()}}.
edit_message(Uid, ChannelIdBin, MessageIdBin, NewContent) ->
    StartMs = elib_dt:millisecond(),
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    MessageId = decode_positive_id(MessageIdBin),
    Result =
        case ChannelId of
            0 ->
                {error, <<"频道不存在"/utf8>>};
            _ when MessageId =:= 0 ->
                {error, <<"消息不存在"/utf8>>};
            _ when NewContent =:= <<>> ->
                {error, <<"消息内容不能为空"/utf8>>};
            _ ->
                case channel_message_ds:find_by_id(MessageId) of
                    Message when is_map(Message) ->
                        do_edit_message(Uid, ChannelId, MessageId, NewContent, Message);
                    _ ->
                        {error, <<"消息不存在"/utf8>>}
                end
        end,
    channel_logic_common:log_channel_action(
        Uid, ChannelId, MessageId, <<"edit_message">>, Result, StartMs
    ),
    Result.

-spec do_edit_message(integer(), integer(), integer(), binary(), map()) ->
    ok | {error, binary() | {integer(), binary()}}.
do_edit_message(Uid, ChannelId, MessageId, NewContent, Message) ->
    MessageChannelId = maps:get(<<"channel_id">>, Message, 0),
    MessageAuthorId = maps:get(<<"author_id">>, Message, 0),
    ValidMessage =
        is_integer(MessageChannelId) andalso
            MessageChannelId =:= ChannelId andalso
            is_integer(MessageAuthorId) andalso
            MessageAuthorId > 0,
    Revoked = maps:get(<<"revoked">>, Message, false),
    case true of
        _ when not ValidMessage ->
            {error, <<"消息不存在"/utf8>>};
        %% 编辑与撤回语义不同：仅作者本人可编辑，管理员不可
        _ when MessageAuthorId =/= Uid ->
            {error, <<"无权限编辑此消息"/utf8>>};
        _ when Revoked =:= true ->
            {error, <<"消息已撤回，无法编辑"/utf8>>};
        _ ->
            case is_within_edit_window(Message) of
                false ->
                    {error, <<"编辑时间已超出限制"/utf8>>};
                true ->
                    inspect_edit(Uid, ChannelId, MessageId, NewContent, Message)
            end
    end.

inspect_edit(Uid, ChannelId, MessageId, NewContent, Message) ->
    MsgType = maps:get(<<"msg_type">>, Message, <<"text">>),
    Payload = maps:get(<<"payload">>, Message, #{}),
    case moderation_business:inspect_channel(ChannelId, MsgType, NewContent, Payload) of
        {blocked, _} ->
            {error, <<"内容包含违规词汇，保存失败"/utf8>>};
        {error, Reason} ->
            {error, Reason};
        Verdict ->
            EditedAt = elib_dt:now(),
            Version = moderation_business:new_version(),
            Data = #{
                content => NewContent,
                edited_at => EditedAt,
                updated_at => EditedAt,
                moderation_version => Version
            },
            case channel_message_ds:update_checked(MessageId, Data, Message) of
                {ok, 1} ->
                    maybe_enqueue_review(
                        moderation_business:bind_version(Verdict, Version),
                        MessageId,
                        ChannelId,
                        Uid,
                        NewContent
                    ),
                    channel_logic_notify:notify_message_edited(
                        ChannelId, MessageId, NewContent, EditedAt
                    ),
                    ok;
                {ok, 0} ->
                    {error, <<"内容已变化，请重新编辑"/utf8>>};
                {error, {Code, Msg}} when is_integer(Code) -> {error, {Code, Msg}};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

-spec get_admins(integer() | binary()) -> {ok, list(map())} | {error, binary()}.
get_admins(ChannelId) when is_binary(ChannelId) ->
    DecodedChannelId = decode_positive_id(ChannelId),
    case DecodedChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            get_admins(DecodedChannelId)
    end;
get_admins(ChannelId) when not is_integer(ChannelId); ChannelId =< 0 ->
    {error, <<"频道不存在"/utf8>>};
get_admins(ChannelId) ->
    case channel_admin_ds:list_by_channel(ChannelId) of
        {ok, Admins} when is_list(Admins) ->
            {ok, [A || A <- Admins, is_map(A)]};
        {ok, UnexpectedAdmins} ->
            {error, elib_cnv:safe_to_binary(UnexpectedAdmins)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec update_admin_role(integer(), integer() | binary(), integer(), integer()) ->
    ok | {error, binary() | {integer(), binary()}}.
update_admin_role(Uid, ChannelId, TargetUid, Role) when is_binary(ChannelId) ->
    case decode_positive_id(ChannelId) of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        DecodedChannelId ->
            update_admin_role(Uid, DecodedChannelId, TargetUid, Role)
    end;
update_admin_role(_Uid, ChannelId, _TargetUid, _Role) when
    not is_integer(ChannelId); ChannelId =< 0
->
    {error, <<"频道不存在"/utf8>>};
update_admin_role(Uid, ChannelId, TargetUid, Role) ->
    case channel_logic_common:get_user_role(ChannelId, Uid) of
        3 ->
            %% T7 归档写守卫（R3 #13 收口）：守卫已下沉 channel_admin_ds 写事务
            case channel_admin_ds:update_role(ChannelId, TargetUid, Role) of
                {ok, _} ->
                    %% ENT-BE-01：治理成功路径 best-effort 通知目标用户角色已变更
                    channel_logic_notify:notify_admin_role_updated(ChannelId, TargetUid, Role),
                    ok;
                %% 稳定错误码（980 等）原样透传供 handler envelope 映射
                {error, {Code, Msg}} when is_integer(Code) -> {error, {Code, Msg}};
                {error, _} ->
                    {error, <<"更新角色失败"/utf8>>}
            end;
        _ ->
            {error, <<"无权限操作，仅创建者可修改角色"/utf8>>}
    end.

%% @doc 为一页消息批量补充当前用户已添加的反应类型列表（my_reactions）
%% 一次 IN 查询覆盖整页消息，禁止逐条查询（N+1）。
%% 查询失败时记录日志并降级为全空列表，不阻断消息列表返回。
-spec attach_my_reactions(integer(), list(map())) -> list(map()).
attach_my_reactions(_Uid, []) ->
    [];
attach_my_reactions(Uid, Messages) ->
    MsgIds = [Id || #{<<"id">> := Id} <- Messages, is_integer(Id)],
    ByMsg =
        try channel_ds:list_user_reactions(Uid, MsgIds) of
            {ok, Rows} when is_list(Rows) ->
                lists:foldl(
                    fun
                        (#{<<"message_id">> := Mid, <<"reaction_type">> := Rt}, Acc) ->
                            maps:update_with(Mid, fun(L) -> L ++ [Rt] end, [Rt], Acc);
                        (_, Acc) ->
                            Acc
                    end,
                    #{},
                    Rows
                );
            {error, Reason} ->
                ?ERROR_LOG(["channel_list_user_reactions_failed", Uid, Reason]),
                #{};
            Unexpected ->
                ?ERROR_LOG(["channel_list_user_reactions_invalid", Uid, Unexpected]),
                #{}
        catch
            _:Reason ->
                ?ERROR_LOG(["channel_list_user_reactions_crashed", Uid, Reason]),
                #{}
        end,
    [
        M#{<<"my_reactions">> => maps:get(maps:get(<<"id">>, M, 0), ByMsg, [])}
     || M <- Messages
    ].

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    try ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    catch
        _:_ -> 0
    end.

-spec push_unread_updates(integer()) -> ok.
push_unread_updates(ChannelId) ->
    case channel_subscription_ds:list_unread_counts_by_channel(ChannelId) of
        {ok, Rows} when is_list(Rows) ->
            lists:foreach(
                fun(Row) ->
                    case
                        {
                            maps:get(<<"user_id">>, Row, 0),
                            maps:get(<<"unread_count">>, Row, 0)
                        }
                    of
                        {Uid, Count} when is_integer(Uid), Uid > 0, is_integer(Count), Count >= 0 ->
                            channel_logic_notify:notify_channel_unread_count(ChannelId, Uid, Count);
                        _ ->
                            ok
                    end
                end,
                Rows
            ),
            ok;
        _ ->
            ok
    end.

-spec is_within_revoke_window(map()) -> boolean().
is_within_revoke_window(Message) ->
    is_within_window(Message, channel_logic_common:channel_revoke_window_seconds()).

-spec is_within_edit_window(map()) -> boolean().
is_within_edit_window(Message) ->
    is_within_window(Message, channel_logic_common:channel_edit_window_seconds()).

-spec is_within_window(map(), integer()) -> boolean().
is_within_window(Message, WindowSecs) ->
    case WindowSecs =< 0 of
        true ->
            true;
        false ->
            CreatedAt = maps:get(<<"created_at">>, Message, undefined),
            CreatedAtMs =
                try elib_dt:rfc3339_to(CreatedAt, millisecond) of
                    Value when is_integer(Value) ->
                        Value;
                    _ ->
                        0
                catch
                    _:_ -> 0
                end,
            NowMs = elib_dt:millisecond(),
            is_integer(CreatedAtMs) andalso
                CreatedAtMs > 0 andalso
                NowMs >= CreatedAtMs andalso
                (NowMs - CreatedAtMs =< WindowSecs * 1000)
    end.

-spec safe_subscriber_uids(integer()) -> list(integer()).
safe_subscriber_uids(ChannelId) ->
    try channel_ds:subscriber_uids(ChannelId) of
        Uids when is_list(Uids) ->
            Uids;
        _ ->
            []
    catch
        _:_ -> []
    end.
