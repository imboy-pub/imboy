-module(user_deletion_executor).
%%%===================================================================
%%% @doc 账号删除执行器（Implementation Plan Task D-03）
%%%
%%% 按 data-disposition.yml 的 delete 决策执行有序删除：
%%%   1. 所有权转移（群/工作区/频道，D-02 默认：转移给继任成员，
%%%      无继任则删除/关闭）
%%%   2. 主事务：声明式有序删除（消息/朋友圈/频道/项目/工作区/群/
%%%      AI-Bot/反馈等个人数据）+ 复用核心 20 表删除 + 用户主行最后
%%%   3. 事务后：Garage S3 附件对象删除（外部资源；DB 侧引用已清，
%%%      失败重试幂等）
%%%
%%% 幂等契约：任意步骤对"已删除"的行是 no-op，两轮执行终态一致。
%%% v1 明示不做（记入 manifest 机制列）：msg_store_seq（会话计数器，
%%% 无用户列）、verification_code（时效自失效）；资金/审计类按 D-02
%%% 为 retain，不在执行清单。
%%%===================================================================

-export([collect_attachment_keys_tx/2]).
-export([transfer_ownerships_tx/2]).
-export([execute_main_tx/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 事务内收集用户附件对象键（Garage 删除用），并删除附件行
-spec collect_attachment_keys_tx(pid(), integer()) -> {ok, [binary()]} | {error, term()}.
collect_attachment_keys_tx(Conn, Uid) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT path FROM public.attachment WHERE creator_user_id = $1">>,
            [Uid]
        )
    of
        {ok, Rows} ->
            Keys = [maps:get(<<"path">>, R) || R <- Rows, is_map_key(<<"path">>, R)],
            {ok, _} = elib_pg:execute(
                Conn,
                <<"DELETE FROM public.attachment WHERE creator_user_id = $1">>,
                [Uid]
            ),
            {ok, Keys};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内所有权转移/关闭（D-02 默认策略）
%%   群：继任成员（最早入群的其他成员）；无继任 → 删群及群域子表
%%   工作区：继任成员；无继任 → 归档（status=archived）
%%   频道：继任管理员；无继任 → 删频道及频道消息/订阅
-spec transfer_ownerships_tx(pid(), integer()) -> ok | {error, term()}.
transfer_ownerships_tx(Conn, Uid) ->
    %% 群
    ok = transfer_generic(
        Conn,
        Uid,
        <<
            "SELECT gm.user_id FROM public.group_member gm"
            " JOIN public.\"group\" g ON g.id = gm.group_id"
            " WHERE g.owner_uid = $1 AND gm.user_id <> $1"
            " ORDER BY gm.created_at ASC LIMIT 1"
        >>,
        <<"SELECT id FROM public.\"group\" WHERE owner_uid = $1">>,
        fun(C, ObjectId, Successor) ->
            {ok, _} = elib_pg:execute(
                C,
                <<
                    "UPDATE public.\"group\" SET owner_uid = $1"
                    " WHERE id = $2"
                >>,
                [Successor, ObjectId]
            ),
            ok
        end,
        fun(C, ObjectId) ->
            lists:foreach(
                fun(Table) ->
                    {ok, _} = elib_pg:execute(
                        C,
                        <<"DELETE FROM public.", (atom_to_binary(Table, utf8))/binary,
                            " WHERE group_id = $1">>,
                        [ObjectId]
                    )
                end,
                [
                    group_member,
                    group_notice,
                    group_log,
                    group_album,
                    group_file,
                    group_schedule,
                    group_vote,
                    group_task,
                    group_tag,
                    fts_group
                ]
            ),
            {ok, _} = elib_pg:execute(
                C,
                <<"DELETE FROM public.\"group\" WHERE id = $1">>,
                [ObjectId]
            ),
            ok
        end
    ),
    %% 工作区：无继任 → 归档（不级联删除项目，保留协作历史只读）
    ok = transfer_generic(
        Conn,
        Uid,
        <<
            "SELECT wm.user_id FROM public.workspace_member wm"
            " JOIN public.workspace w ON w.id = wm.workspace_id"
            " WHERE w.owner_id = $1 AND wm.user_id <> $1"
            " ORDER BY wm.joined_at ASC LIMIT 1"
        >>,
        <<"SELECT id FROM public.workspace WHERE owner_id = $1">>,
        fun(C, ObjectId, Successor) ->
            {ok, _} = elib_pg:execute(
                C,
                <<"UPDATE public.workspace SET owner_id = $1 WHERE id = $2">>,
                [Successor, ObjectId]
            ),
            ok
        end,
        fun(C, ObjectId) ->
            {ok, _} = elib_pg:execute(
                C,
                <<
                    "UPDATE public.workspace SET status = 'archived',"
                    " archived_at = CURRENT_TIMESTAMP WHERE id = $1"
                >>,
                [ObjectId]
            ),
            ok
        end
    ),
    %% 频道：无继任管理员 → 删频道与频道消息/订阅
    ok = transfer_generic(
        Conn,
        Uid,
        <<
            "SELECT ca.user_id FROM public.channel_admin ca"
            " JOIN public.channel c ON c.id = ca.channel_id"
            " WHERE c.creator_uid = $1 AND ca.user_id <> $1"
            " ORDER BY ca.created_at ASC LIMIT 1"
        >>,
        <<"SELECT id FROM public.channel WHERE creator_uid = $1">>,
        fun(C, ObjectId, Successor) ->
            {ok, _} = elib_pg:execute(
                C,
                <<"UPDATE public.channel SET creator_uid = $1 WHERE id = $2">>,
                [Successor, ObjectId]
            ),
            ok
        end,
        fun(C, ObjectId) ->
            {ok, _} = elib_pg:execute(
                C,
                <<"DELETE FROM public.channel_message WHERE channel_id = $1">>,
                [ObjectId]
            ),
            {ok, _} = elib_pg:execute(
                C,
                <<"DELETE FROM public.channel_subscription WHERE channel_id = $1">>,
                [ObjectId]
            ),
            {ok, _} = elib_pg:execute(
                C,
                <<"DELETE FROM public.channel WHERE id = $1">>,
                [ObjectId]
            ),
            ok
        end
    ).

%% @doc 主删除事务：核心 20 表（含会话令牌/设备/E2EE/Olm 密钥）
%% + 声明式扩展清单 + 用户主行最后（user_ds 负责主行）
-spec execute_main_tx(pid(), integer()) -> ok.
execute_main_tx(Conn, Uid) ->
    %% mcp_client_grant 无用户列：先按用户 client 子查询删除
    {ok, _} = elib_pg:execute(
        Conn,
        <<
            "DELETE FROM public.mcp_client_grant WHERE client_id IN"
            " (SELECT client_id FROM public.mcp_client WHERE owner_uid = $1)"
        >>,
        [Uid]
    ),
    ok = user_ds:delete_all_related_data(Conn, Uid),
    lists:foreach(
        fun({Table, Cols}) ->
            delete_any_of(Conn, Table, Cols, Uid)
        end,
        delete_spec()
    ),
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

%% 通用所有权转移：对每个 owned 对象找继任 → 转移；无继任 → Close
transfer_generic(Conn, Uid, SuccessorSql, OwnedSql, Transfer, Close) ->
    {ok, Owned} = elib_pg:query(Conn, OwnedSql, [Uid]),
    lists:foreach(
        fun(#{<<"id">> := ObjectId}) ->
            case elib_pg:query(Conn, SuccessorSql, [Uid]) of
                {ok, [#{<<"user_id">> := Successor}]} ->
                    Transfer(Conn, ObjectId, Successor);
                _ ->
                    Close(Conn, ObjectId)
            end
        end,
        Owned
    ),
    ok.

%% 声明式删除清单（D-02 delete 决策的执行映射）
%% 每项 {Table, Cols}：DELETE WHERE 任一列 = Uid。顺序 = 依赖序。
%% 用户主行不在清单：由 user_ds:delete_all_related_data 最后删除。
delete_spec() ->
    [
        %% 会话域
        {<<"conversation">>, [<<"user_id">>]},
        {<<"conversation_delete">>, [<<"user_id">>]},
        {<<"conversation_pin">>, [<<"user_id">>]},
        %% 消息域
        {<<"msg_delivery">>, [<<"to_uid">>]},
        {<<"msg_mention">>, [<<"from_uid">>, <<"mentioned_uid">>]},
        {<<"msg_read">>, [<<"from_uid">>, <<"to_uid">>]},
        {<<"msg_reaction">>, [<<"user_id">>]},
        {<<"msg_forward">>, [<<"forward_from_id">>, <<"forward_to_id">>, <<"original_from_id">>]},
        {<<"msg_c2c">>, [<<"from_id">>, <<"to_id">>]},
        {<<"msg_c2g">>, [<<"from_id">>]},
        {<<"msg_c2s">>, [<<"from_id">>]},
        {<<"msg_s2c">>, [<<"from_id">>, <<"to_id">>]},
        {<<"msg_store">>, [<<"from_id">>, <<"to_id">>]},
        {<<"msg_store_staging">>, [<<"from_id">>]},
        {<<"msg_topic">>, [<<"user_id">>, <<"to_id">>]},
        %% 朋友圈域
        {<<"moment_comment">>, [<<"user_id">>]},
        {<<"moment_like">>, [<<"user_id">>]},
        {<<"moment_post_acl">>, [<<"uid">>]},
        {<<"moment_timeline">>, [<<"author_uid">>, <<"recipient_uid">>]},
        {<<"moment_post">>, [<<"author_uid">>]},
        %% 频道域（用户维度；owned 频道已在转移/关闭阶段处置）
        {<<"channel_message_view">>, [<<"user_id">>]},
        {<<"channel_comment">>, [<<"user_id">>]},
        {<<"channel_reaction">>, [<<"user_id">>]},
        {<<"channel_subscription">>, [<<"user_id">>]},
        {<<"channel_invitation">>, [<<"inviter_uid">>, <<"invitee_uid">>]},
        {<<"channel_webhook">>, [<<"creator_uid">>]},
        {<<"channel_message">>, [<<"author_id">>]},
        {<<"channel_admin">>, [<<"user_id">>]},
        %% 项目域
        {<<"project_task">>, [<<"assignee_id">>, <<"creator_id">>]},
        {<<"project_event">>, [<<"actor_id">>]},
        {<<"project_member">>, [<<"user_id">>]},
        {<<"project_channel_rel">>, [<<"created_by">>]},
        {<<"project">>, [<<"owner_id">>]},
        %% 工作区（个人维度；owned 已转移/归档）
        {<<"workspace_invite">>, [<<"created_by">>]},
        {<<"workspace_member">>, [<<"user_id">>]},
        %% 群域（个人维度）
        {<<"group_album_photo_comment">>, [<<"user_id">>]},
        {<<"group_album_photo_like">>, [<<"user_id">>]},
        {<<"group_schedule_participant">>, [<<"user_id">>]},
        {<<"group_schedule_remind">>, [<<"user_id">>]},
        {<<"group_vote_record">>, [<<"user_id">>]},
        {<<"group_task_assignment">>, [<<"user_id">>]},
        {<<"group_notice">>, [<<"user_id">>, <<"edit_user_id">>]},
        {<<"group_log">>, [<<"option_uid">>]},
        %% 红包/财务/审计行按 D-02 为 retain，不在执行清单
        {<<"live_room">>, [<<"user_id">>]},
        {<<"push_token">>, [<<"user_id">>]},
        {<<"sso_identity">>, [<<"uid">>]},
        {<<"agent_payment_mandate">>, [<<"owner_uid">>]},
        {<<"attach_pending">>, [<<"creator_user_id">>]},
        {<<"doc_draft_sections">>, [<<"user_id">>]},
        {<<"feedback_reply">>, [<<"replier_user_id">>]},
        {<<"feedback">>, [<<"user_id">>]},
        %% AI/Bot 域（grants 先于 client 删除）
        {<<"bot_oauth_grant">>, [<<"user_id">>]},
        {<<"bot">>, [<<"owner_uid">>]},
        {<<"ai_agent">>, [<<"owner_uid">>]},
        {<<"mcp_client">>, [<<"owner_uid">>]}
    ].

delete_any_of(Conn, Table, Cols, Uid) ->
    %% 归属列有 bigint 也有 varchar：统一按列::text = 文本比较，
    %% 一次性的账号删除场景无索引性能压力
    Conds = [<<"(", Col/binary, "::text = $1)">> || Col <- Cols],
    Where = iolist_to_binary(lists:join(<<" OR ">>, Conds)),
    Sql = <<"DELETE FROM public.", Table/binary, " WHERE ", Where/binary>>,
    UidText = integer_to_binary(Uid),
    case elib_pg:execute(Conn, Sql, [UidText]) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({delete_failed, Table, Reason})
    end.
