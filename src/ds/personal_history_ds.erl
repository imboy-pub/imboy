-module(personal_history_ds).
%%%
%%% personal_history_ds —— 个人消息历史域 DS 层（MH-01 合同 v1.1 / MH-02）
%%%
%%% 集中三件事：
%%% 1. 部署实例域标识（§0 三元隔离：账号+域+代次）；
%%% 2. 会话参与关系判定（conversation_id 与投递链 conv_key 同源，§10.2）；
%%% 3. 事务窗口组合（W1 upload 单事务 / W2 cursor 只在归档 commit 后发放 /
%%%    W3 purge 可重入 / W4 旧代次写 410）。
%%%
%%% 错误以机器码原子返回（logic 层映射 HTTP 码）：
%%% not_enabled | generation_expired | purge_in_progress | version_conflict |
%%% nonce_reused | key_package_missing | not_participant | bad_conversation |
%%% page_token_expired | rescan_bound_exceeded | transient_failure
%%%

-export([
    domain/0,
    parse_conversation/1,
    canonical_conversation/1,
    is_participant/2,
    % 授权
    opt_in/3,
    disable/2,
    purge_online/3,
    % 归档
    upload/3,
    mark_deleted/4,
    % 变更流
    issue_changes/5,
    ack_cursor/4,
    ack_cursor/5,
    % 分页快照
    page/5,
    encode_page_token/3,
    decode_page_token/2,
    % 密钥包
    put_key_package/5,
    % 常量（handler/openapi 对齐用）
    page_size/0,
    change_window/0
]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% §10.2 change log 有界窗口：窗口外游标压缩为 reset_hint（重扫走 page 链）。
-define(CHANGE_WINDOW, 10000).
%% page 快照单页条数（含墓碑行）。
-define(PAGE_SIZE, 50).
%% §10.2 page_token 链长上限（重扫数值界）；超出 410 rescan_bound_exceeded。
-define(PAGE_MAX_HOPS, 100).
%% page_token 有效期；过期 410 page_token_expired 触发有界重扫。
-define(PAGE_TOKEN_TTL_MS, 600000).
%% 单条密文上限（1MB，与 e2ee backup 密文包同量级）。
-define(MAX_CIPHERTEXT_BYTES, 1048576).

%% @doc with_tx 结果归一：epgsql:with_transaction 对 fun 返回 {error,R}
%% 会回滚并改写为 {rollback,R}——本域错误码全为原子，统一还原，
%% 未知回滚原因归 transient_failure（503 安全重试）。
-spec normalize_tx(term()) -> term().
normalize_tx({rollback, Reason}) when is_atom(Reason) -> {error, Reason};
normalize_tx({rollback, _Reason}) -> {error, transient_failure};
normalize_tx(Other) -> Other.

%% ===================================================================
%% 域标识 / 会话（§0 / §10.2）
%% ===================================================================

%% @doc 部署实例标识（三元隔离的"域"）：显式 personal_history_domain 优先，
%% 缺省回落 base_url（站点身份），再回落环境名。集中在此，禁止散取。
-spec domain() -> binary().
domain() ->
    case config_ds:env(personal_history_domain, <<>>) of
        <<>> ->
            case config_ds:env(base_url, <<>>) of
                <<>> ->
                    atom_to_binary(imboy_env:current(), utf8);
                BaseUrl ->
                    BaseUrl
            end;
        Explicit ->
            Explicit
    end.

%% @doc 解析 conversation_id（与投递链 conv_key 同源：c2c:{min}:{max}）。
%% 只认 C2C（§0：群/系统/客服/企业消息不在本域，fail-closed）。
-spec parse_conversation(binary()) ->
    {ok, c2c, integer(), integer()} | {error, bad_conversation}.
parse_conversation(<<"c2c:", Rest/binary>>) when byte_size(Rest) > 0 ->
    case binary:split(Rest, <<":">>) of
        [A, B] when byte_size(A) > 0, byte_size(B) > 0 ->
            try
                {ok, c2c, binary_to_integer(A), binary_to_integer(B)}
            catch
                _:_ -> {error, bad_conversation}
            end;
        _ ->
            {error, bad_conversation}
    end;
parse_conversation(_) ->
    {error, bad_conversation}.

%% @doc 规范化为服务端权威 conv_key（min:max 排序，与
%% msg_archive_repo:conv_key/3 同构）。
-spec canonical_conversation(binary()) -> {ok, binary()} | {error, bad_conversation}.
canonical_conversation(ConversationId) ->
    case parse_conversation(ConversationId) of
        {ok, c2c, A, B} ->
            Min = erlang:min(A, B),
            Max = erlang:max(A, B),
            {ok, <<"c2c:", (integer_to_binary(Min))/binary, ":", (integer_to_binary(Max))/binary>>};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 参与关系判定（§1 授权四格：仅会话两端可写自己副本）。
-spec is_participant(integer(), binary()) -> boolean().
is_participant(AccountId, ConversationId) ->
    case parse_conversation(ConversationId) of
        {ok, c2c, A, B} -> AccountId =:= A orelse AccountId =:= B;
        {error, bad_conversation} -> false
    end.

%% ===================================================================
%% 授权状态机（§1：OFF → ON(g) → OFF_PURGE_PENDING(g+1) → OFF_PURGED）
%% ===================================================================

%% @doc opt-in（幂等）：无行 → ON(g=1)；ON → 同代次同状态返回；
%% OFF_PURGED → 新代次重开；OFF_PURGE_PENDING → 409 purge_in_progress。
%% 副作用：新代次起不回填（§2.2）。
-spec opt_in(integer(), binary(), binary()) ->
    {ok, map(), created | idempotent | reenabled} | {error, atom()}.
opt_in(AccountId, Domain, ClientInstanceId) ->
    normalize_tx(
        elib_pg:with_tx(fun(Conn) -> opt_in_tx(Conn, AccountId, Domain, ClientInstanceId) end)
    ).

opt_in_tx(Conn, AccountId, Domain, ClientInstanceId) ->
    case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
        {ok, undefined} ->
            insert_first_enable(Conn, AccountId, Domain, ClientInstanceId);
        {ok, #{<<"state">> := <<"ON">>} = Row} ->
            {ok, Row, idempotent};
        {ok, #{<<"state">> := <<"OFF_PURGE_PENDING">>}} ->
            {error, purge_in_progress};
        {ok, #{<<"state">> := <<"OFF">>, <<"generation">> := G}} ->
            reenable(Conn, AccountId, Domain, ClientInstanceId, G);
        {ok, #{<<"state">> := <<"OFF_PURGED">>, <<"generation">> := G}} ->
            reenable(Conn, AccountId, Domain, ClientInstanceId, G);
        {error, Reason} ->
            {error, Reason}
    end.

insert_first_enable(Conn, AccountId, Domain, ClientInstanceId) ->
    case
        personal_history_repo:insert_authorization_tx(Conn, AccountId, Domain, 1, ClientInstanceId)
    of
        {ok, Row} -> {ok, Row, created};
        {error, Reason} -> {error, Reason}
    end.

reenable(Conn, AccountId, Domain, ClientInstanceId, FromGeneration) ->
    case
        personal_history_repo:enable_authorization_tx(
            Conn, AccountId, Domain, FromGeneration, ClientInstanceId
        )
    of
        {ok, Row} -> {ok, Row, reenabled};
        {error, Reason} -> {error, Reason}
    end.

%% @doc disable 第一步（W3）：ON → OFF_PURGE_PENDING 原子 generation+1；
%% 崩溃残留的 OFF_PURGE_PENDING 重入返回同代次（可重入，不二次提升）。
%% 返回 purge 目标代次（g+1）。
-spec disable(integer(), binary()) -> {ok, map(), integer()} | {error, atom()}.
disable(AccountId, Domain) ->
    normalize_tx(elib_pg:with_tx(fun(Conn) -> disable_tx(Conn, AccountId, Domain) end)).

disable_tx(Conn, AccountId, Domain) ->
    case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
        {ok, #{<<"state">> := <<"ON">>, <<"generation">> := G}} ->
            case personal_history_repo:cas_disable_tx(Conn, AccountId, Domain, G) of
                {ok, #{<<"generation">> := NewG} = Row} -> {ok, Row, NewG};
                {ok, none} -> {error, not_enabled};
                {error, Reason} -> {error, Reason}
            end;
        {ok, #{<<"state">> := <<"OFF_PURGE_PENDING">>, <<"generation">> := G}} ->
            {ok, #{<<"state">> => <<"OFF_PURGE_PENDING">>, <<"generation">> => G}, G};
        {ok, _} ->
            {error, not_enabled};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc disable 第二步（W3 删账）：归档密文 + 游标 + 本用途密钥封装销毁，
%% 完成落账 OFF_PURGED（防复活标记 = 授权行 account/domain/generation/
%% purged_at）。单事务原子；失败可整段重放（replay ledger）。
-spec purge_online(integer(), binary(), integer()) -> {ok, map()} | {error, atom()}.
purge_online(AccountId, Domain, Generation) ->
    normalize_tx(
        elib_pg:with_tx(fun(Conn) ->
            case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
                {ok, #{<<"state">> := <<"OFF_PURGE_PENDING">>, <<"generation">> := G}} when
                    G =:= Generation
                ->
                    purge_rows(Conn, AccountId, Domain, G);
                {ok, #{<<"state">> := <<"OFF_PURGED">>} = Row} ->
                    {ok, Row#{<<"phase">> => online_done}};
                {ok, _} ->
                    {error, not_enabled};
                {error, Reason} ->
                    {error, Reason}
            end
        end)
    ).

purge_rows(Conn, AccountId, Domain, Generation) ->
    {ok, _} = personal_history_repo:delete_archive_by_account_tx(Conn, AccountId, Domain),
    {ok, _} = personal_history_repo:delete_cursors_by_account_tx(Conn, AccountId, Domain),
    {ok, _} = personal_history_repo:delete_key_packages_by_account_tx(Conn, AccountId, Domain),
    ok = personal_history_repo:mark_purged_tx(Conn, AccountId, Domain, Generation),
    {ok, #{
        <<"state">> => <<"OFF_PURGED">>,
        <<"generation">> => Generation,
        <<"phase">> => online_done
    }}.

%% ===================================================================
%% upload（§2.3 / W1 / W4）
%% ===================================================================

%% @doc 归档一条个人历史密文。Item 需含：
%% conversation_id（规范化 conv_key）、msg_id、action_version、conv_seq、
%% generation（客户端视角代次，可选整数）、key_version、nonce（binary）、
%% ciphertext（binary）、envelope_meta（jsonb map）。
%%
%% W1：幂等键与归档在同一事务；授权行 FOR UPDATE 锁定后校验 ON+代次。
%% W4：授权代次 ≠ 客户端视角（或已关闭）→ generation_expired。
%% 幂等：语义键冲突按 action_version 分类（= 重试原样返回 / < 409 / > 终态推进）。
%% nonce（account,domain,key_version,nonce 唯一）重用 → nonce_reused。
%%
%% 事务内不撞 23505 中止（除 nonce 竞态残余）：语义键经 ON CONFLICT DO
%% NOTHING 吸收；nonce 先 FOR UPDATE 预检。残余 nonce 竞态让事务回滚，
%% 归一为 {error, nonce_reused}（客户端安全重试）。
-spec upload(integer(), binary(), map()) ->
    {ok, stored | duplicate | advanced, map(), integer()} | {error, atom()}.
upload(AccountId, Domain, Item) ->
    normalize_tx(elib_pg:with_tx(fun(Conn) -> upload_tx(Conn, AccountId, Domain, Item) end)).

upload_tx(Conn, AccountId, Domain, #{msg_id := MsgId} = Item) ->
    ok = c2c_action_receipt_repo:lock_original_tx(Conn, MsgId),
    case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
        {ok, #{<<"state">> := <<"ON">>, <<"generation">> := G}} ->
            guard_generation(G, Item, Conn, AccountId, Domain);
        {ok, #{<<"state">> := State}} when
            State =:= <<"OFF_PURGE_PENDING">>; State =:= <<"OFF_PURGED">>
        ->
            {error, generation_expired};
        {ok, _} ->
            {error, not_enabled};
        {error, Reason} ->
            {error, Reason}
    end.

%% W4：客户端视角代次（Item.generation，可选）与授权代次不一致 → 410。
guard_generation(G, Item, Conn, AccountId, Domain) ->
    ClientGen = maps:get(generation, Item, undefined),
    case ClientGen =:= undefined orelse ClientGen =:= G of
        true -> guard_key_package(Item, Conn, AccountId, Domain, G);
        false -> {error, generation_expired}
    end.

%% §2.3 422：上传所用 key_version 的用途密钥包必须已就绪（fail-closed）。
guard_key_package(Item, Conn, AccountId, Domain, G) ->
    KeyVersion = maps:get(key_version, Item),
    case personal_history_repo:find_key_package_tx(Conn, AccountId, Domain, KeyVersion) of
        {ok, undefined} -> {error, key_package_missing};
        {ok, _} -> guard_message_fact(Item, Conn, AccountId, Domain, G);
        {error, Reason} -> {error, Reason}
    end.

guard_message_fact(
    #{conversation_id := Conv, msg_id := MsgId, conv_seq := Seq, action_version := V} = Item,
    Conn,
    AccountId,
    Domain,
    G
) ->
    case personal_history_repo:find_accepted_message_tx(Conn, AccountId, Domain, Conv, MsgId) of
        {ok, #{<<"conv_seq">> := Seq, <<"action_version">> := V}} ->
            insert_archive(Item, Conn, AccountId, Domain, G);
        {ok, undefined} ->
            {error, backfill_evidence_missing};
        {ok, _} ->
            {error, version_conflict};
        {error, Reason} ->
            {error, Reason}
    end.

insert_archive(Item, Conn, AccountId, Domain, G) ->
    Row = archive_row(AccountId, Domain, G, Item),
    KeyVersion = maps:get(key_version, Item),
    Nonce = maps:get(nonce, Item),
    RowMsgId = maps:get(msg_id, Row),
    case
        personal_history_repo:find_msg_id_by_nonce_tx(Conn, AccountId, Domain, KeyVersion, Nonce)
    of
        {ok, undefined} ->
            insert_new_archive(Conn, AccountId, Domain, Row, G);
        {ok, #{<<"msg_id">> := RowMsgId}} ->
            %% 同 nonce 同语义键：仅同版本重试幂等；换版本必须换 nonce
            %%（§10.4 随机 nonce 禁重用，fail-closed）
            classify_same_nonce(Conn, AccountId, Domain, Row);
        {ok, _Other} ->
            %% nonce 已被其他语义记录占用（§10.4）
            {error, nonce_reused};
        {error, Reason} ->
            {error, Reason}
    end.

archive_row(AccountId, Domain, G, Item) ->
    #{
        account_id => AccountId,
        domain => Domain,
        conversation_id => maps:get(conversation_id, Item),
        msg_id => maps:get(msg_id, Item),
        action_version => maps:get(action_version, Item),
        conv_seq => maps:get(conv_seq, Item),
        generation => G,
        key_version => maps:get(key_version, Item),
        nonce => maps:get(nonce, Item),
        ciphertext => maps:get(ciphertext, Item),
        envelope_meta => maps:get(envelope_meta, Item)
    }.

insert_new_archive(Conn, AccountId, Domain, Row, G) ->
    case personal_history_repo:insert_archive_tx(Conn, Row) of
        {ok, exists} ->
            %% 语义键已存在且 nonce 未占用：按 action_version 定胜负
            classify_semantic(Conn, AccountId, Domain, Row);
        {ok, #{<<"event_seq">> := _} = Inserted} ->
            {ok, stored, Inserted, G};
        {error, unique_violation} ->
            %% 预检后残余 nonce 竞态：事务已中止，停止后续查询
            {error, nonce_reused};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 同 nonce 同语义键分类：同版本重试幂等返回原结果；旧版本迟到 409；
%% 更高版本沿用旧 nonce = nonce 重用 → 409（新密文必须新 nonce）。
classify_same_nonce(Conn, AccountId, Domain, Row) ->
    ConvId = maps:get(conversation_id, Row),
    MsgId = maps:get(msg_id, Row),
    case personal_history_repo:find_archive_tx(Conn, AccountId, Domain, ConvId, MsgId) of
        {ok, #{<<"action_version">> := ExistingV} = Existing} ->
            IncomingV = maps:get(action_version, Row),
            same_nonce_verdict(Existing, ExistingV, IncomingV);
        {ok, undefined} ->
            {error, transient_failure};
        {error, Reason} ->
            {error, Reason}
    end.

same_nonce_verdict(Existing, V, V) ->
    {ok, duplicate, Existing, maps:get(<<"generation">>, Existing)};
same_nonce_verdict(_Existing, ExistingV, IncomingV) when ExistingV > IncomingV ->
    {error, version_conflict};
same_nonce_verdict(_Existing, _ExistingV, _IncomingV) ->
    %% 更高版本复用旧 nonce：nonce 重用拒收
    {error, nonce_reused}.

%% @doc 语义键冲突分类：同 action_version 重试返回原结果不覆盖；
%% 旧版本迟到 409；更高版本 = 编辑/撤回终态推进。
classify_semantic(Conn, AccountId, Domain, Row) ->
    ConvId = maps:get(conversation_id, Row),
    MsgId = maps:get(msg_id, Row),
    case personal_history_repo:find_archive_tx(Conn, AccountId, Domain, ConvId, MsgId) of
        {ok, undefined} ->
            {error, transient_failure};
        {ok, #{<<"action_version">> := ExistingV} = Existing} ->
            IncomingV = maps:get(action_version, Row),
            classify_version(Conn, Existing, ExistingV, IncomingV, Row);
        {error, Reason} ->
            {error, Reason}
    end.

classify_version(_Conn, Existing, V, V, _Row) ->
    %% 同版本重试：返回原结果不覆盖（§2.3 幂等）
    {ok, duplicate, Existing, maps:get(<<"generation">>, Existing)};
classify_version(_Conn, _Existing, ExistingV, IncomingV, _Row) when ExistingV > IncomingV ->
    %% 旧版本迟到：不得按重试覆盖已有终态
    {error, version_conflict};
classify_version(Conn, _Existing, _ExistingV, _IncomingV, Row) ->
    advance_terminal(Conn, Row).

%% 编辑/撤回终态：action_version 单调推进才允许更新（不按到达时间覆盖）
advance_terminal(Conn, Row) ->
    case personal_history_repo:advance_archive_tx(Conn, Row) of
        {ok, none} ->
            %% 已被并发更高版本推进：迟到旧版本不得覆盖
            {error, version_conflict};
        {ok, #{<<"event_seq">> := _} = Advanced} ->
            {ok, advanced, Advanced, maps:get(generation, Row)};
        {error, unique_violation} ->
            %% 预检后残余 nonce 竞态：事务已中止
            {error, nonce_reused};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 墓碑（MSG-H15）：撤回终态在归档侧的投影（保留行，deleted_at 置位）。
-spec mark_deleted(integer(), binary(), binary(), binary()) ->
    {ok, non_neg_integer()} | {error, atom()}.
mark_deleted(AccountId, Domain, ConversationId, MsgId) ->
    normalize_tx(
        elib_pg:with_tx(fun(Conn) ->
            case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
                {ok, #{<<"state">> := <<"ON">>}} ->
                    personal_history_repo:mark_deleted_tx(
                        Conn, AccountId, Domain, ConversationId, MsgId
                    );
                {ok, _} ->
                    {ok, 0};
                {error, _} = Error ->
                    Error
            end
        end)
    ).

%% ===================================================================
%% changes / ack-cursor（§2.4 / §10.2 / W2）
%% ===================================================================

%% @doc 发放变更事件：event_seq 单调锚（W2：只读已 commit 行，commit 前
%% 的上传不可见）。游标落后窗口（§10.2 有界）→ reset_hint 触发 page 重扫。
%% 返回 {ok, Generation, Events, NextCursor | undefined, ResetHint}
-spec issue_changes(integer(), binary(), binary(), integer(), pos_integer()) ->
    {ok, integer(), [map()], integer() | undefined, boolean()} | {error, atom()}.
issue_changes(AccountId, Domain, ClientInstance, Cursor, Limit) ->
    normalize_tx(
        elib_pg:with_tx(fun(Conn) ->
            issue_changes_tx(Conn, AccountId, Domain, ClientInstance, Cursor, Limit)
        end)
    ).

issue_changes_tx(Conn, AccountId, Domain, ClientInstance, Cursor, Limit) ->
    case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
        {ok, #{<<"state">> := <<"ON">>, <<"generation">> := G}} ->
            ok = personal_history_repo:ensure_cursor_tx(Conn, AccountId, Domain, G, ClientInstance),
            case personal_history_repo:find_cursor_tx(Conn, AccountId, Domain, G, ClientInstance) of
                {ok, #{<<"change_cursor">> := Issued}} when Cursor =< Issued ->
                    issue_window(Conn, AccountId, Domain, G, ClientInstance, Cursor, Limit);
                {ok, _} ->
                    {ok, G, [], undefined, true};
                {error, _} = Error ->
                    Error
            end;
        {ok, #{<<"state">> := State}} when
            State =:= <<"OFF_PURGE_PENDING">>; State =:= <<"OFF_PURGED">>
        ->
            {error, generation_expired};
        {ok, _} ->
            {error, not_enabled};
        {error, Reason} ->
            {error, Reason}
    end.

issue_window(Conn, AccountId, Domain, G, ClientInstance, Cursor, Limit) ->
    {ok, OutsideWindow} = personal_history_repo:changes_exceed_window_tx(
        Conn, AccountId, Domain, G, Cursor, ?CHANGE_WINDOW
    ),
    case OutsideWindow of
        true ->
            %% 窗口外：压缩为 tombstone+水位语义 → 客户端全量重扫
            {ok, G, [], undefined, true};
        false ->
            {ok, Events} = personal_history_repo:changes_since_tx(
                Conn, AccountId, Domain, G, Cursor, Limit
            ),
            issue_mark(Conn, AccountId, Domain, G, ClientInstance, Events)
    end.

issue_mark(_Conn, _AccountId, _Domain, _G, _ClientInstance, []) ->
    {ok, _G, [], undefined, false};
issue_mark(Conn, AccountId, Domain, G, ClientInstance, Events) ->
    NextCursor = maps:get(<<"event_seq">>, lists:last(Events)),
    ok = personal_history_repo:record_issued_tx(
        Conn, AccountId, Domain, G, ClientInstance, NextCursor
    ),
    {ok, G, Events, NextCursor, false}.

%% @doc durably-ack：仅推进 ≤ 服务端已发放最大值（§10.2）；幂等（重复
%% ack 同值不动水位，W5：重复 ACK 不影响历史）。
%% 返回 {ok, Generation, Accepted, Acked, Issued}
-spec ack_cursor(integer(), binary(), binary(), integer()) ->
    {ok, integer(), boolean(), integer(), integer()} | {error, atom()}.
ack_cursor(AccountId, Domain, ClientInstance, Cursor) ->
    ack_cursor(AccountId, Domain, ClientInstance, Cursor, undefined).

-spec ack_cursor(integer(), binary(), binary(), integer(), pos_integer() | undefined) ->
    {ok, integer(), boolean(), integer(), integer()} | {error, atom()}.
ack_cursor(AccountId, Domain, ClientInstance, Cursor, ExpectedGeneration) ->
    normalize_tx(
        elib_pg:with_tx(fun(Conn) ->
            ack_authorized_cursor(
                Conn, AccountId, Domain, ClientInstance, Cursor, ExpectedGeneration
            )
        end)
    ).

ack_authorized_cursor(Conn, AccountId, Domain, ClientInstance, Cursor, ExpectedGeneration) ->
    case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
        {ok, #{<<"state">> := <<"ON">>, <<"generation">> := G}} when
            ExpectedGeneration =:= undefined; ExpectedGeneration =:= G
        ->
            ack_cursor_tx(Conn, AccountId, Domain, G, ClientInstance, Cursor);
        {ok, #{<<"state">> := <<"ON">>}} ->
            {error, generation_expired};
        {ok, #{<<"state">> := State}} when
            State =:= <<"OFF_PURGE_PENDING">>; State =:= <<"OFF_PURGED">>
        ->
            {error, generation_expired};
        {ok, _} ->
            {error, not_enabled};
        {error, Reason} ->
            {error, Reason}
    end.

ack_cursor_tx(Conn, AccountId, Domain, G, ClientInstance, Cursor) ->
    ok = personal_history_repo:ensure_cursor_tx(Conn, AccountId, Domain, G, ClientInstance),
    case personal_history_repo:ack_cursor_tx(Conn, AccountId, Domain, G, ClientInstance, Cursor) of
        {ok, #{<<"acked_cursor">> := Acked, <<"change_cursor">> := Issued}} ->
            {ok, G, true, Acked, Issued};
        {ok, none} ->
            ack_reject(Conn, AccountId, Domain, G, ClientInstance, Cursor);
        {error, Reason} ->
            {error, Reason}
    end.

ack_reject(Conn, AccountId, Domain, G, ClientInstance, _Cursor) ->
    {ok, Slot} = personal_history_repo:find_cursor_tx(
        Conn, AccountId, Domain, G, ClientInstance
    ),
    case Slot of
        undefined ->
            {ok, G, false, 0, 0};
        #{<<"acked_cursor">> := Acked, <<"change_cursor">> := Issued} ->
            {ok, G, false, Acked, Issued}
    end.

%% ===================================================================
%% page 分页快照（§2.4 / §10.2 重扫数值界）
%% ===================================================================

%% @doc 分页快照：conv_seq DESC，含墓碑（deleted_at 非空 → tombstone 投影）。
%% Token 存在则校验（代次/时效/链长），并以其 Before 锚定；
%% 否则以 Before0（0 = 最新）起扫，签发新 token。
%% 返回 {ok, Generation, Rows, NextToken | undefined, HasMore}
-spec page(integer(), binary(), binary(), integer(), binary() | undefined) ->
    {ok, integer(), [map()], binary() | undefined, boolean()} | {error, atom()}.
page(AccountId, Domain, ConversationId, Before0, Token) ->
    case personal_history_repo:find_authorization(AccountId, Domain) of
        {ok, #{<<"state">> := <<"ON">>, <<"generation">> := G}} ->
            page_authorized(AccountId, Domain, ConversationId, Before0, Token, G);
        {ok, #{<<"state">> := State}} when
            State =:= <<"OFF_PURGE_PENDING">>; State =:= <<"OFF_PURGED">>
        ->
            %% §2.6 disable 后拒绝旧页令牌/恢复
            {error, generation_expired};
        {ok, _} ->
            {error, not_enabled};
        {error, Reason} ->
            {error, Reason}
    end.

page_authorized(AccountId, Domain, ConversationId, Before0, Token, G) ->
    case resolve_page_anchor(Token, G, Before0) of
        {ok, Hop, Before} ->
            {ok, Rows} = personal_history_repo:page_archive(
                AccountId, Domain, G, ConversationId, Before, ?PAGE_SIZE
            ),
            build_page_result(Rows, G, Hop);
        {error, Reason} ->
            {error, Reason}
    end.

resolve_page_anchor(undefined, _G, Before0) ->
    {ok, 0, normalize_before(Before0)};
resolve_page_anchor(Token, G, _Before0) ->
    decode_page_token(Token, G).

normalize_before(Before) when is_integer(Before), Before >= 0 -> Before;
normalize_before(_) -> 0.

build_page_result([], G, _Hop) ->
    {ok, G, [], undefined, false};
build_page_result(Rows, G, Hop) ->
    HasMore = length(Rows) =:= ?PAGE_SIZE,
    LastBefore = maps:get(<<"conv_seq">>, lists:last(Rows)),
    NextToken =
        case HasMore of
            true -> encode_page_token(G, Hop + 1, LastBefore);
            false -> undefined
        end,
    {ok, G, Rows, NextToken, HasMore}.

%% @doc page_token：v1:<generation>:<hop>:<before>:<issued_ms> 的 base64url。
%% 无签名：token 只承载分页锚点，每次请求仍过授权与代次校验，伪造无增益。
-spec encode_page_token(integer(), non_neg_integer(), integer()) -> binary().
encode_page_token(Generation, Hop, Before) ->
    NowMs = erlang:system_time(millisecond),
    Plain =
        <<"v1:", (integer_to_binary(Generation))/binary, ":", (integer_to_binary(Hop))/binary, ":",
            (integer_to_binary(Before))/binary, ":", (integer_to_binary(NowMs))/binary>>,
    base64url_encode(Plain).

%% @doc 校验并解码 page_token：代次不符 → generation_expired（410）；
%% 过期 → page_token_expired（410）；链长超界 → rescan_bound_exceeded（410）。
-spec decode_page_token(binary(), integer()) ->
    {ok, non_neg_integer(), integer()} | {error, atom()}.
decode_page_token(Token, CurrentGeneration) ->
    case binary:split(base64url_decode(Token), <<":">>, [global]) of
        [<<"v1">>, GenBin, HopBin, BeforeBin, IssuedBin] ->
            decode_page_fields(
                CurrentGeneration,
                binary_to_integer_safe(GenBin),
                binary_to_integer_safe(HopBin),
                binary_to_integer_safe(BeforeBin),
                binary_to_integer_safe(IssuedBin)
            );
        _ ->
            {error, page_token_expired}
    end.

decode_page_fields(CurrentGen, {ok, Gen}, {ok, Hop}, {ok, _Before}, {ok, _IssuedMs}) when
    Gen =:= CurrentGen, Hop > ?PAGE_MAX_HOPS
->
    {error, rescan_bound_exceeded};
decode_page_fields(CurrentGen, {ok, Gen}, {ok, Hop}, {ok, Before}, {ok, IssuedMs}) when
    Gen =:= CurrentGen
->
    Age = erlang:system_time(millisecond) - IssuedMs,
    case Age =< ?PAGE_TOKEN_TTL_MS of
        true -> {ok, Hop, Before};
        false -> {error, page_token_expired}
    end;
decode_page_fields(_CurrentGen, {ok, _OtherGen}, _Hop, _Before, _Issued) ->
    {error, generation_expired};
decode_page_fields(_CurrentGen, _Gen, _Hop, _Before, _Issued) ->
    {error, page_token_expired}.

binary_to_integer_safe(Bin) ->
    try
        {ok, binary_to_integer(Bin)}
    catch
        _:_ -> {error, bad_integer}
    end.

base64url_encode(Plain) ->
    Trimmed = base64:encode(Plain),
    <<<<(url_char(C))>> || <<C>> <= Trimmed>>.

url_char($+) -> $-;
url_char($/) -> $_;
url_char(C) -> C.

base64url_decode(Token) ->
    Restored = <<<<(std_char(C))>> || <<C>> <= Token>>,
    Padded = pad_base64(Restored),
    try
        base64:decode(Padded)
    catch
        _:_ -> <<>>
    end.

std_char($-) -> $+;
std_char($_) -> $/;
std_char(C) -> C.

pad_base64(Bin) ->
    case byte_size(Bin) rem 4 of
        2 -> <<Bin/binary, "==">>;
        3 -> <<Bin/binary, "=">>;
        _ -> Bin
    end.

%% ===================================================================
%% key package（§2.5 / §10.4）
%% ===================================================================

%% @doc 存用途独立密钥包（仅版本化 wrapped 密文+KDF 元数据）。
%% uq_phk_version 冲突 → version_conflict；DB 故障 → transient_failure。
-spec put_key_package(integer(), binary(), integer(), integer(), map()) ->
    {ok, map()} | {error, atom()}.
put_key_package(AccountId, Domain, KeyVersion, Generation, Pkg) ->
    Row = #{
        account_id => AccountId,
        domain => Domain,
        key_version => KeyVersion,
        generation => Generation,
        wrapped_key => maps:get(wrapped_key, Pkg),
        kdf_meta => maps:get(kdf_meta, Pkg),
        integrity_hash => maps:get(integrity_hash, Pkg)
    },
    normalize_tx(
        elib_pg:with_tx(fun(Conn) ->
            case personal_history_repo:lock_authorization_tx(Conn, AccountId, Domain) of
                {ok, #{<<"state">> := <<"ON">>, <<"generation">> := Generation}} ->
                    insert_key_package_tx(Conn, Row);
                {ok, #{<<"generation">> := _}} ->
                    {error, generation_expired};
                {ok, _} ->
                    {error, not_enabled};
                {error, _} ->
                    {error, transient_failure}
            end
        end)
    ).

insert_key_package_tx(Conn, Row) ->
    case personal_history_repo:insert_key_package(Conn, Row) of
        {ok, Inserted} -> {ok, Inserted};
        {error, unique_violation} -> {error, version_conflict};
        {error, _Reason} -> {error, transient_failure}
    end.

%% ===================================================================
%% 常量导出（handler/openapi 契约对齐）
%% ===================================================================

-spec page_size() -> pos_integer().
page_size() -> ?PAGE_SIZE.

-spec change_window() -> pos_integer().
change_window() -> ?CHANGE_WINDOW.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
