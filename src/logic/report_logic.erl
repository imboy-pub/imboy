-module(report_logic).

%% Stable ops_governance report domain boundary.

-export([create/5]).
-export([create_message/6]).
-export([admin_list/5]).
-export([admin_detail/2]).
-export([admin_resolve/5]).
-export([admin_batch_resolve/5]).

-include("error_code.hrl").
-include("log.hrl").

%% R-01: 消息举报 reason 白名单（客户端 complaintReason 枚举 + 常见扩展）。
-define(REPORT_MESSAGE_REASONS, [
    <<"spam">>,
    <<"harassment">>,
    <<"inappropriate">>,
    <<"pornography">>,
    <<"fraud">>,
    <<"violence">>,
    <<"illegal">>,
    <<"other">>
]).

-spec create(integer(), term(), term(), term(), term()) -> {ok, map()} | {error, binary()}.
create(ReporterUid, TargetTypeRaw, TargetIdRaw, ReasonRaw, DescRaw) when
    is_integer(ReporterUid), ReporterUid > 0
->
    TargetType = normalize_target_type(TargetTypeRaw, undefined),
    TargetId = decode_positive_id(TargetIdRaw),
    Reason = normalize_text(ReasonRaw, 64),
    Desc = normalize_text(DescRaw, 500),
    case {TargetType, TargetId > 0, Reason =/= <<>>} of
        {undefined, _, _} ->
            {error, <<"举报类型无效"/utf8>>};
        {_, false, _} ->
            {error, <<"举报对象无效"/utf8>>};
        {_, _, false} ->
            {error, <<"举报原因不能为空"/utf8>>};
        {<<"moment">>, true, true} ->
            moment_logic:report_post(ReporterUid, TargetId, Reason, Desc);
        {Type, true, true} ->
            case report_ticket_ds:create(Type, TargetId, ReporterUid, Reason, Desc) of
                {ok, ReportId} ->
                    {ok, #{
                        <<"report_id">> => safe_encode(ReportId),
                        <<"target_type">> => Type
                    }};
                {error, already_reported} ->
                    {error, <<"您已举报过该对象"/utf8>>};
                {error, _Reason} ->
                    {error, <<"举报失败"/utf8>>}
            end
    end;
create(_, _, _, _, _) ->
    {error, <<"举报参数无效"/utf8>>}.

%% R-01: 消息类一等举报。举报对象必须是真实存在的 C2C/C2G/频道消息，
%% 举报人必须对该消息有可见权（收发双方/群成员/频道订阅者或公开频道访客），
%% 消息 ID 与 scope 由服务端核验，不落自由文本。E2EE 消息只接受举报人
%% 明确同意后提交的最小明文摘录 + 哈希/上下文元数据；服务端不解密、
%% 不索取会话密钥、不读取无关消息。
-spec create_message(integer(), term(), term(), term(), term(), term()) ->
    {ok, map()} | {error, binary()}.
create_message(ReporterUid, ChatTypeRaw, TargetIdRaw, ScopeIdRaw, ReasonRaw, EvidenceRaw) when
    is_integer(ReporterUid), ReporterUid > 0
->
    case normalize_chat_type(ChatTypeRaw) of
        undefined ->
            {error, <<"举报类型无效"/utf8>>};
        ChatType ->
            create_message_checked(
                ReporterUid, ChatType, TargetIdRaw, ScopeIdRaw, ReasonRaw, EvidenceRaw
            )
    end;
create_message(_, _, _, _, _, _) ->
    {error, <<"举报参数无效"/utf8>>}.

-spec create_message_checked(integer(), c2c | c2g | channel, term(), term(), term(), term()) ->
    {ok, map()} | {error, binary()}.
create_message_checked(ReporterUid, ChatType, TargetIdRaw, ScopeIdRaw, ReasonRaw, EvidenceRaw) ->
    ScopeId = decode_positive_id(ScopeIdRaw),
    Reason = normalize_text(ReasonRaw, 64),
    Evidence0 = normalize_evidence_input(EvidenceRaw),
    %% 客户端契约：c2c/c2g 携带 msg_id 字符串（Flutter Message.id）；
    %% channel 携带服务端行 bigint ID。
    {TargetKey, KeyValid} =
        case ChatType of
            channel ->
                RowId = decode_positive_id(TargetIdRaw),
                {integer_to_binary(RowId), RowId > 0};
            _ ->
                MsgId = normalize_text(TargetIdRaw, 50),
                {MsgId, MsgId =/= <<>>}
        end,
    case {KeyValid, ScopeId > 0, lists:member(Reason, ?REPORT_MESSAGE_REASONS)} of
        {false, _, _} ->
            {error, <<"举报对象无效"/utf8>>};
        {_, false, _} ->
            {error, <<"举报参数无效"/utf8>>};
        {_, _, false} ->
            {error, <<"举报原因无效"/utf8>>};
        _ ->
            case ensure_report_rate(ReporterUid) of
                ok ->
                    create_message_verified(
                        ReporterUid, ChatType, TargetKey, ScopeId, Reason, Evidence0
                    );
                {error, Msg} ->
                    {error, Msg}
            end
    end.

-spec create_message_verified(integer(), c2c | c2g | channel, binary(), integer(), binary(), map()) ->
    {ok, map()} | {error, binary()}.
create_message_verified(ReporterUid, ChatType, TargetKey, ScopeId, Reason, Evidence0) ->
    case report_ticket_ds:fetch_message_target(ChatType, TargetKey) of
        {error, not_found} ->
            {error, <<"举报对象不存在或已被删除"/utf8>>};
        {error, _Reason} ->
            {error, <<"举报失败"/utf8>>};
        {ok, Target} ->
            case message_target_state(Target) of
                deleted ->
                    {error, <<"举报对象不存在或已被删除"/utf8>>};
                ContentState ->
                    create_message_visible(
                        ReporterUid, ChatType, ScopeId, Reason, Evidence0, Target, ContentState
                    )
            end
    end.

-spec create_message_visible(
    integer(), c2c | c2g | channel, integer(), binary(), map(), map(), present | edited
) ->
    {ok, map()} | {error, binary()}.
create_message_visible(ReporterUid, ChatType, ScopeId, Reason, Evidence0, Target, ContentState) ->
    case
        {
            ensure_scope(ChatType, ScopeId, Target),
            ensure_visible(ReporterUid, ChatType, ScopeId, Target)
        }
    of
        {ok, ok} ->
            IsE2EE = maps:get(e2ee, Target, false),
            Excerpt = maps:get(<<"content_excerpt">>, Evidence0, <<>>),
            case
                IsE2EE andalso Excerpt =/= <<>> andalso
                    maps:get(<<"e2ee_consent">>, Evidence0, false) =/= true
            of
                true ->
                    {error, <<"提交加密消息内容需要您的明确同意"/utf8>>};
                false ->
                    AuthorId = maps:get(author_id, Target, 0),
                    %% 落库 target_id 用服务端核验出的行 bigint ID（稳定目标），
                    %% 客户端 msg_id 保留在 evidence。
                    ServerRowId = maps:get(server_id, Target, 0),
                    Evidence = finalize_evidence(Evidence0, Target, IsE2EE, ContentState),
                    Desc = <<>>,
                    case
                        report_ticket_ds:create_message(
                            ServerRowId,
                            atom_to_binary(ChatType, utf8),
                            ScopeId,
                            AuthorId,
                            ReporterUid,
                            Reason,
                            Desc,
                            Evidence,
                            #{}
                        )
                    of
                        {ok, ReportId} ->
                            {ok, #{
                                <<"report_id">> => safe_encode(ReportId),
                                <<"target_type">> => <<"message">>,
                                <<"target_sub_type">> => atom_to_binary(ChatType, utf8)
                            }};
                        {error, already_reported} ->
                            {error, <<"您已举报过该对象"/utf8>>};
                        {error, _Reason} ->
                            {error, <<"举报失败"/utf8>>}
                    end
            end;
        {{error, Msg}, _} ->
            {error, Msg};
        {_, {error, Msg}} ->
            {error, Msg}
    end.

%% R-01: 管理端工单详情——只返回该举报工单自身绑定的结构化证据，
%% 不提供按消息 ID 的任意浏览入口；权限门在 handler（reports:read）。
-spec admin_detail(integer(), term()) -> {ok, map()} | {error, binary()}.
admin_detail(_AdmUid, ReportIdRaw) ->
    ReportId = decode_positive_id(ReportIdRaw),
    case ReportId > 0 of
        false ->
            {error, <<"举报参数无效"/utf8>>};
        true ->
            case report_ticket_ds:find_by_id(ReportId) of
                Row when is_map(Row), map_size(Row) > 0 ->
                    {ok, detail_transfer(Row)};
                _ ->
                    {error, <<"举报记录不存在"/utf8>>}
            end
    end.

-spec admin_list(term(), integer(), integer(), integer(), map()) -> {ok, map()} | {error, binary()}.
admin_list(TargetTypeRaw, Status, Page, Size, Filter) ->
    TargetType = normalize_target_type(TargetTypeRaw, <<"moment">>),
    Page2 = clamp(Page, 1, 1000000),
    Size2 = clamp(Size, 1, 100),
    case TargetType of
        undefined ->
            {error, <<"举报类型无效"/utf8>>};
        <<"moment">> ->
            case moment_logic:admin_list_reports(Status, Page2, Size2) of
                {ok, Payload0} ->
                    {ok, normalize_moment_payload(Payload0)};
                {error, Msg} ->
                    {error, Msg}
            end;
        Type ->
            TargetId = decode_positive_id(maps:get(target_id, Filter, <<>>)),
            ReporterUid = decode_positive_id(maps:get(reporter_uid, Filter, <<>>)),
            Keyword = normalize_text(maps:get(keyword, Filter, <<>>), 128),
            RepoFilter = #{
                status => Status,
                target_type => Type,
                target_id => TargetId,
                reporter_uid => ReporterUid,
                keyword => Keyword
            },
            case report_ticket_ds:page_admin(RepoFilter, Page2, Size2) of
                {ok, Payload0} ->
                    List = maps:get(list, Payload0, []),
                    List2 = [report_transfer(Item) || Item <- List],
                    Payload = Payload0#{
                        target_type => Type,
                        list => List2,
                        items => List2
                    },
                    {ok, Payload};
                {error, _Reason} ->
                    {error, <<"查询失败"/utf8>>}
            end
    end.

-spec admin_resolve(integer(), term(), term(), integer(), term()) -> ok | {error, binary()}.
admin_resolve(AdmUid, TargetTypeRaw, ReportIdRaw, Result, NoteRaw) when
    is_integer(AdmUid), AdmUid > 0
->
    TargetType = normalize_target_type(TargetTypeRaw, <<>>),
    Note = normalize_text(NoteRaw, 500),
    case lists:member(Result, [1, 2]) of
        false ->
            {error, <<"举报参数无效"/utf8>>};
        true ->
            case TargetType of
                <<"moment">> ->
                    moment_logic:admin_resolve_report(AdmUid, ReportIdRaw, Result, Note);
                _ ->
                    resolve_non_moment(AdmUid, TargetType, ReportIdRaw, Result, Note)
            end
    end;
admin_resolve(_, _, _, _, _) ->
    {error, <<"举报参数无效"/utf8>>}.

-spec admin_batch_resolve(integer(), term(), list(), integer(), term()) ->
    {ok, map()} | {error, binary()}.
admin_batch_resolve(AdmUid, TargetTypeRaw, ReportIds, Result, NoteRaw) ->
    TargetType = normalize_target_type(TargetTypeRaw, <<"moment">>),
    case {is_list(ReportIds), lists:member(Result, [1, 2])} of
        {false, _} ->
            {error, <<"report_ids is empty"/utf8>>};
        {_, false} ->
            {error, <<"result must be 1 or 2"/utf8>>};
        _ ->
            {SuccessCount, FailedIds0} =
                lists:foldl(
                    fun(ReportId, {Succ, FailedAcc}) ->
                        case admin_resolve(AdmUid, TargetType, ReportId, Result, NoteRaw) of
                            ok ->
                                {Succ + 1, FailedAcc};
                            {error, _} ->
                                {Succ, [ReportId | FailedAcc]}
                        end
                    end,
                    {0, []},
                    ReportIds
                ),
            FailedIds = lists:reverse(FailedIds0),
            {ok, #{
                <<"success_count">> => SuccessCount,
                <<"failed_count">> => length(FailedIds),
                <<"failed_ids">> => [to_binary(Id) || Id <- FailedIds]
            }}
    end.

-spec resolve_non_moment(integer(), binary(), term(), integer(), binary()) ->
    ok | {error, binary()}.
resolve_non_moment(AdmUid, TargetType, ReportIdRaw, Result, Note) ->
    ReportId = decode_positive_id(ReportIdRaw),
    case ReportId > 0 of
        false ->
            {error, <<"举报参数无效"/utf8>>};
        true ->
            case report_ticket_ds:find_by_id(ReportId) of
                Row when is_map(Row), map_size(Row) > 0 ->
                    StoredType = normalize_target_type(
                        maps:get(<<"target_type">>, Row, <<>>), undefined
                    ),
                    case TargetType =:= <<>> orelse TargetType =:= StoredType of
                        false ->
                            {error, <<"举报类型不匹配"/utf8>>};
                        true ->
                            case report_ticket_ds:resolve(ReportId, Result, Note, AdmUid) of
                                {ok, N} when N > 0 ->
                                    case
                                        report_action_log_ds:create(ReportId, AdmUid, Result, Note)
                                    of
                                        {ok, _} ->
                                            ok;
                                        {error, LogReason} ->
                                            ?ERROR_LOG([
                                                "report_action_log_failed",
                                                ReportId,
                                                AdmUid,
                                                LogReason
                                            ])
                                    end,
                                    ok;
                                {ok, _} ->
                                    {error, <<"举报记录不存在"/utf8>>};
                                {error, _Reason} ->
                                    {error, <<"处理举报失败"/utf8>>}
                            end
                    end;
                _ ->
                    {error, <<"举报记录不存在"/utf8>>}
            end
    end.

-spec normalize_moment_payload(map()) -> map().
normalize_moment_payload(Payload0) ->
    List = maps:get(list, Payload0, []),
    List2 = [normalize_moment_item(Item) || Item <- List],
    Payload0#{
        target_type => <<"moment">>,
        list => List2,
        items => maps:get(items, Payload0, List2)
    }.

-spec normalize_moment_item(map()) -> map().
normalize_moment_item(Item) ->
    TargetId = maps:get(<<"target_id">>, Item, maps:get(<<"post_id">>, Item, <<>>)),
    Item#{
        <<"target_type">> => <<"moment">>,
        <<"target_id">> => TargetId
    }.

-spec report_transfer(map()) -> map().
report_transfer(Report) ->
    ReportId = to_int(maps:get(<<"id">>, Report, 0)),
    TargetId = to_int(maps:get(<<"target_id">>, Report, 0)),
    ReporterUid = to_int(maps:get(<<"reporter_uid">>, Report, 0)),
    HandledBy = to_int(maps:get(<<"handled_by">>, Report, 0)),
    Report#{
        <<"id">> => safe_encode(ReportId),
        <<"target_id">> => safe_encode(TargetId),
        <<"reporter_uid">> => safe_encode(ReporterUid),
        <<"handled_by">> => safe_encode(HandledBy),
        <<"evidence">> => decode_evidence(maps:get(<<"evidence">>, Report, #{}))
    }.

-spec normalize_target_type(term(), binary() | undefined) -> binary() | undefined.
normalize_target_type(Value, Default) when is_binary(Value) ->
    normalize_target_type_binary(trim_binary(Value), Default);
normalize_target_type(Value, Default) when is_list(Value) ->
    normalize_target_type_binary(trim_binary(unicode:characters_to_binary(Value)), Default);
normalize_target_type(_, Default) ->
    Default.

-spec normalize_target_type_binary(binary(), binary() | undefined) -> binary() | undefined.
normalize_target_type_binary(<<>>, Default) ->
    Default;
normalize_target_type_binary(Value, Default) ->
    Lower = string:lowercase(binary_to_list(Value)),
    case Lower of
        "moment" -> <<"moment">>;
        "moments" -> <<"moment">>;
        "group" -> <<"group">>;
        "groups" -> <<"group">>;
        "channel" -> <<"channel">>;
        "channels" -> <<"channel">>;
        "user" -> <<"user">>;
        "users" -> <<"user">>;
        "message" -> <<"message">>;
        "messages" -> <<"message">>;
        _ -> Default
    end.

%% ==================== R-01 message report helpers ====================

-spec normalize_chat_type(term()) -> c2c | c2g | channel | undefined.
normalize_chat_type(Value) when is_binary(Value) ->
    normalize_chat_type_bin(string:lowercase(binary_to_list(Value)));
normalize_chat_type(Value) when is_list(Value) ->
    normalize_chat_type_bin(string:lowercase(Value));
normalize_chat_type(Value) when is_atom(Value) ->
    normalize_chat_type_bin(atom_to_list(Value));
normalize_chat_type(_) ->
    undefined.

-spec normalize_chat_type_bin(string()) -> c2c | c2g | channel | undefined.
normalize_chat_type_bin("c2c") -> c2c;
normalize_chat_type_bin("c2g") -> c2g;
normalize_chat_type_bin("channel") -> channel;
normalize_chat_type_bin(_) -> undefined.

%% 复用 agent_rate_limiter 固定窗口（不另造限流框架）：per-reporter
%% 每 scope 总量双闸门，默认 30 次/分钟/用户、600 次/分钟/全域。
-spec ensure_report_rate(integer()) -> ok | {error, binary()}.
ensure_report_rate(ReporterUid) ->
    case agent_rate_limiter:allow(<<"report_create">>, ReporterUid) of
        allow ->
            ok;
        {deny, _Which} ->
            {error, <<"举报过于频繁，请稍后再试"/utf8>>}
    end.

%% 举报时点目标状态：channel 撤回/下架=deleted（拒绝举报）；
%% channel 编辑过=edited（允许，证据留痕）；c2c/c2g 物理删除即查无此行。
-spec message_target_state(map()) -> present | edited | deleted.
message_target_state(Target) ->
    Revoked = maps:get(revoked, Target, false) =:= true,
    Status = maps:get(status, Target, 1),
    case Revoked orelse (is_integer(Status) andalso Status =/= 1) of
        true ->
            deleted;
        false ->
            case maps:get(edited_at, Target, null) of
                null -> present;
                _ -> edited
            end
    end.

%% scope 一致性：客户端申报的 scope 必须真实指向该消息所属会话/频道。
%% c2c 的 scope 为对话任一端 uid；c2g 为群 id；channel 为频道 id（严格等值）。
-spec ensure_scope(c2c | c2g | channel, integer(), map()) -> ok | {error, binary()}.
ensure_scope(c2c, ScopeId, Target) ->
    FromId = maps:get(from_id, Target, 0),
    ToId = maps:get(to_id, Target, 0),
    case lists:member(ScopeId, [FromId, ToId]) of
        true -> ok;
        false -> {error, <<"举报参数无效"/utf8>>}
    end;
ensure_scope(_, ScopeId, Target) ->
    case ScopeId =:= maps:get(scope_id, Target, -1) of
        true -> ok;
        false -> {error, <<"举报参数无效"/utf8>>}
    end.

%% 举报人可见性（IDOR fail-closed）：c2c 限收发双方；c2g 限在职群成员；
%% channel 限订阅者或公开频道（status=1 且 visibility=0）访客。
-spec ensure_visible(integer(), c2c | c2g | channel, integer(), map()) -> ok | {error, binary()}.
ensure_visible(ReporterUid, c2c, _ScopeId, Target) ->
    FromId = maps:get(from_id, Target, 0),
    ToId = maps:get(to_id, Target, 0),
    case lists:member(ReporterUid, [FromId, ToId]) of
        true -> ok;
        false -> {error, <<"无权举报该消息"/utf8>>}
    end;
ensure_visible(ReporterUid, c2g, ScopeId, _Target) ->
    case group_member_ds:is_member(ScopeId, ReporterUid) of
        true -> ok;
        false -> {error, <<"无权举报该消息"/utf8>>}
    end;
ensure_visible(ReporterUid, channel, ScopeId, Target) ->
    Public = maps:get(channel_public, Target, false),
    %% orelse 短路：公开频道访客不触发订阅查询
    CanSee = Public orelse channel_subscription_ds:is_subscribed(ScopeId, ReporterUid),
    case CanSee of
        true -> ok;
        false -> {error, <<"无权举报该消息"/utf8>>}
    end.

%% evidence 入参白名单归一：handler 传来的可能是 map（JSON body）或
%% binary（字符串化 JSON），非白名单字段一律丢弃。
-spec normalize_evidence_input(term()) -> map().
normalize_evidence_input(Raw) when is_map(Raw) ->
    pick_evidence_fields(Raw);
normalize_evidence_input(Raw) when is_binary(Raw); is_list(Raw) ->
    Bin = iolist_to_binary(Raw),
    Decoded =
        try
            jsx:decode(Bin, [return_maps])
        catch
            _:_ -> invalid
        end,
    case Decoded of
        Map when is_map(Map) ->
            pick_evidence_fields(Map);
        _ ->
            #{}
    end;
normalize_evidence_input(_) ->
    #{}.

-spec pick_evidence_fields(map()) -> map().
pick_evidence_fields(Raw) ->
    Excerpt = normalize_text(maps:get(<<"content_excerpt">>, Raw, <<>>), 500),
    ContentHash = normalize_hash(maps:get(<<"content_hash">>, Raw, <<>>)),
    ClientMsgId = normalize_text(maps:get(<<"client_msg_id">>, Raw, <<>>), 50),
    MsgType = normalize_text(maps:get(<<"msg_type">>, Raw, <<>>), 50),
    SentAt = to_int(maps:get(<<"sent_at">>, Raw, 0)),
    Consent = maps:get(<<"e2ee_consent">>, Raw, false) =:= true,
    maps:filter(
        fun(_K, V) -> V =/= <<>> andalso V =/= 0 andalso V =/= false end,
        #{
            <<"content_excerpt">> => Excerpt,
            <<"content_hash">> => ContentHash,
            <<"client_msg_id">> => ClientMsgId,
            <<"msg_type">> => MsgType,
            <<"sent_at">> => SentAt,
            <<"e2ee_consent">> => Consent
        }
    ).

%% 服务端定稿证据：补充举报时点核验元数据。E2EE 消息不读取 payload
%% （不触碰密文）；非 E2EE 消息记录服务端侧内容哈希供审核对比篡改/编辑。
-spec finalize_evidence(map(), map(), boolean(), present | edited) -> map().
finalize_evidence(Evidence0, Target, IsE2EE, ContentState) ->
    ServerHash =
        case IsE2EE of
            true ->
                <<>>;
            false ->
                Payload = maps:get(payload, Target, <<>>),
                hex_hash(Payload)
        end,
    Base = #{
        <<"e2ee">> => IsE2EE,
        <<"content_state">> => atom_to_binary(ContentState, utf8),
        <<"server_msg_id">> => integer_to_binary(maps:get(server_id, Target, 0))
    },
    WithHash =
        case ServerHash of
            <<>> -> Base;
            _ -> Base#{<<"server_content_hash">> => ServerHash}
        end,
    maps:merge(WithHash, Evidence0).

%% E2EE 消息即使举报人拒绝提交摘录，也保留密文信封哈希（不解读内容）
%% 作为完整性锚点。
-spec hex_hash(binary()) -> binary().
hex_hash(Bin) when is_binary(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin));
hex_hash(_) ->
    <<>>.

-spec normalize_hash(term()) -> binary().
normalize_hash(Value) ->
    Bin = normalize_text(Value, 64),
    LowerBin = string:lowercase(binary_to_list(Bin)),
    Lower = list_to_binary(LowerBin),
    case is_hex(Lower) of
        true -> Lower;
        false -> <<>>
    end.

-spec is_hex(binary()) -> boolean().
is_hex(<<>>) ->
    false;
is_hex(Bin) when byte_size(Bin) =< 64 ->
    lists:all(
        fun(Char) -> lists:member(Char, "0123456789abcdef") end,
        binary_to_list(Bin)
    );
is_hex(_) ->
    false.

%% 工单详情出参：64 位 ID 保持 JSON integer（TSID 契约），evidence jsonb
%% 统一解码为 map（epgsql 默认把 jsonb 读成 JSON 文本 binary）。
-spec detail_transfer(map()) -> map().
detail_transfer(Row) ->
    ReportId = to_int(maps:get(<<"id">>, Row, 0)),
    TargetId = to_int(maps:get(<<"target_id">>, Row, 0)),
    ScopeId = to_int(maps:get(<<"target_scope_id">>, Row, 0)),
    AuthorId = to_int(maps:get(<<"target_author_id">>, Row, 0)),
    ReporterUid = to_int(maps:get(<<"reporter_uid">>, Row, 0)),
    HandledBy = to_int(maps:get(<<"handled_by">>, Row, 0)),
    Row#{
        <<"id">> => safe_encode(ReportId),
        <<"target_id">> => safe_encode(TargetId),
        <<"target_scope_id">> => safe_encode(ScopeId),
        <<"target_author_id">> => safe_encode(AuthorId),
        <<"reporter_uid">> => safe_encode(ReporterUid),
        <<"handled_by">> => safe_encode(HandledBy),
        <<"evidence">> => decode_evidence(maps:get(<<"evidence">>, Row, #{}))
    }.

-spec decode_evidence(term()) -> map().
decode_evidence(Map) when is_map(Map) ->
    Map;
decode_evidence(Bin) when is_binary(Bin) ->
    Decoded =
        try
            jsx:decode(Bin, [return_maps])
        catch
            _:_ -> invalid
        end,
    case Decoded of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end;
decode_evidence(_) ->
    #{}.

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    case to_int(Value) of
        Int when Int > 0 ->
            Int;
        _ ->
            case Value of
                Bin when is_binary(Bin), Bin =/= <<>> ->
                    safe_hash_decode(Bin);
                List when is_list(List), List =/= [] ->
                    safe_hash_decode(unicode:characters_to_binary(List));
                _ ->
                    0
            end
    end.

-spec safe_hash_decode(binary()) -> integer().
safe_hash_decode(Hash) ->
    try ec_cnv:to_integer(Hash) of
        Int when is_integer(Int), Int > 0 ->
            Int;
        _ ->
            0
    catch
        _:_ ->
            0
    end.

-spec normalize_text(term(), integer()) -> binary().
normalize_text(Value, MaxLen) when is_integer(MaxLen), MaxLen > 0 ->
    Bin =
        case Value of
            V when is_binary(V) ->
                V;
            V when is_list(V) ->
                unicode:characters_to_binary(V);
            V ->
                ec_cnv:to_binary(V)
        end,
    Trimmed = trim_binary(Bin),
    case byte_size(Trimmed) =< MaxLen of
        true ->
            Trimmed;
        false ->
            binary:part(Trimmed, 0, MaxLen)
    end.

-spec trim_binary(binary()) -> binary().
trim_binary(Bin) ->
    list_to_binary(string:trim(binary_to_list(Bin))).

-spec to_int(term()) -> integer().
to_int(Value) when is_integer(Value) ->
    Value;
to_int(Value) when is_binary(Value); is_list(Value) ->
    try
        Int = ec_cnv:to_integer(Value),
        if
            is_integer(Int) ->
                Int;
            true ->
                0
        end
    catch
        _:_ ->
            0
    end;
to_int(_) ->
    0.

-spec to_binary(term()) -> binary().
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value);
to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary(Value) ->
    ec_cnv:to_binary(Value).

-spec safe_encode(term()) -> integer().
safe_encode(Int) when is_integer(Int), Int > 0 ->
    Int;
safe_encode(_) ->
    0.

-spec clamp(integer(), integer(), integer()) -> integer().
clamp(Value, Min, _Max) when Value < Min ->
    Min;
clamp(Value, _Min, Max) when Value > Max ->
    Max;
clamp(Value, _Min, _Max) ->
    Value.
