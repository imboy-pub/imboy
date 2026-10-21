-module(talk_record_logic).
-moduledoc "面聊记录业务逻辑（T05）：入参校验、策略比对、幂等指纹；编排委托 talk_record_ds。".
%%%
% talk_record 业务逻辑层（Handler→Logic→DS→Repo 的第二层）。
%
% 校验策略（设计 §4/§8/§11）：
%   * 创建：client_recording_id UUID 必填；notice_ack 必须 true（422）；
%     policy_version 与当前能力面一致（不一致 409 context_changed）；
%     personal 拒绝 organization_id/workspace_id（422）；organization 必填
%     organization_id；source_type 仅接受 live_recording（未来枚举 422）；
%     owner 一律取 JWT 身份，忽略客户端冒充字段。
%   * 编辑：仅接受 title/language/notes；owner/scope 等保留字段出现即 422；
%     If-Match 必填（428），非法形态 422。
%   * 幂等指纹：SHA-256(method ++ path ++ "\n" ++ canonical_json)，
%     canonical_json 复用 enterprise_cursor_v2（freeze §2.5）。
%%%

-export([capabilities/0]).
-export([creation_contexts/3]).
-export([create/3]).
-export([detail/2]).
-export([update/4]).
-export([list/4]).
-export([parse_create/1, parse_recovery_context/1]).
-export([parse_update/1]).
-export([parse_filters/1]).
-export([parse_limit/1]).
-export([parse_if_match/1]).
-export([valid_idempotency_key/1]).
-export([request_digest/1]).
%% T07 采集域
-export([request_digest/3]).
-export([parse_capture_create/1]).
-export([parse_event/1]).
-export([parse_audio_part/1]).
-export([parse_confirm_receipt/1]).
-export([parse_device_session/1]).
-export([parse_abandon/1]).
-export([valid_events/0]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(CREATE_PATH, <<"/api/v1/talk-records">>).
-define(MAX_TITLE_CHARS, 120).
-define(MAX_LANGUAGE_BYTES, 32).
-define(MAX_NOTES_BYTES, 100000).
-define(MAX_QUERY_CHARS, 100).

%% ===================================================================
%% Facade（Handler 入口；业务编排全部在 DS 层）
%% ===================================================================

-spec capabilities() -> map().
capabilities() ->
    talk_record_ds:capabilities().

-spec creation_contexts(integer(), binary() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
creation_contexts(Uid, Cursor, Limit) ->
    talk_record_ds:creation_contexts(Uid, Cursor, Limit).

-spec create(integer(), map(), binary() | undefined) ->
    {ok, created | replay, map(), binary()} | {error, term()}.
create(Uid, Body, IdemKey) ->
    case valid_idempotency_key(IdemKey) of
        false ->
            {error, idempotency_key_required};
        true ->
            create_checked(Uid, Body, IdemKey)
    end.

-spec create_checked(integer(), map(), binary()) ->
    {ok, created | replay, map(), binary()} | {error, term()}.
create_checked(Uid, Body, IdemKey) ->
    case request_digest(Body) of
        {ok, Digest} ->
            case talk_record_recovery_logic:replay(Uid, Body, IdemKey, Digest) of
                miss -> create_fresh(Uid, Body, IdemKey);
                Result -> Result
            end;
        {error, _} ->
            {error, invalid_body}
    end.

create_fresh(Uid, Body, IdemKey) ->
    case parse_create(Body) of
        {ok, Validated} ->
            case request_digest(Body) of
                {ok, Digest} ->
                    talk_record_ds:create(Uid, Validated, IdemKey, Digest);
                {error, non_canonical} ->
                    {error, invalid_body}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec detail(integer(), integer()) -> {ok, map()} | {error, term()}.
detail(Uid, Id) ->
    case talk_record_ds:detail(Uid, Id) of
        {error, not_found} ->
            %% 删除后 owner 白名单分支（§11.3：GET 仅 owner 可读清理状态，
            %% 无正文/标题/参与者；其余人仍 404）——T20
            talk_record_delete_logic:deleted_detail(Uid, Id);
        Other ->
            Other
    end.

-spec update(integer(), integer(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
update(Uid, Id, IfMatch, Body) ->
    case parse_if_match(IfMatch) of
        {ok, ExpectedRevision} ->
            update_parsed(Uid, Id, ExpectedRevision, Body);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_parsed(integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_parsed(Uid, Id, ExpectedRevision, Body) ->
    case parse_update(Body) of
        {ok, Changes} ->
            talk_record_ds:update(Uid, Id, ExpectedRevision, Changes);
        {error, Reason} ->
            {error, Reason}
    end.

-spec list(integer(), map(), binary() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
list(Uid, Filters, Cursor, Limit) ->
    talk_record_ds:list(Uid, Filters, Cursor, Limit).

%% ===================================================================
%% 创建入参解析
%% ===================================================================

%% @doc 创建请求体 → 归一化 map（atom key）；违例返回语义化 reason。
-spec parse_create(map()) -> {ok, map()} | {error, atom()}.
parse_create(Body) when is_map(Body) ->
    Cap = talk_record_ds:capabilities(),
    Checks = [
        fun(B) -> check_uuid(B) end,
        fun(B) -> check_notice_ack(B) end,
        fun(B) -> check_policy_version(B, Cap) end,
        fun(B) -> check_source_type(B) end,
        fun(B) -> check_scope(B, Cap) end,
        fun(B) -> check_text_fields(B) end,
        fun(B) -> check_processing_mode(B) end
    ],
    run_checks(Checks, Body, #{
        title => <<>>,
        language => <<>>,
        processing_mode => <<"record_only">>
    });
parse_create(_) ->
    {error, invalid_body}.

%% 恢复已有记录只解析固定身份；不把新建能力开关当读取权限。
parse_recovery_context(Body) when is_map(Body) ->
    Allowed = #{<<"scopes">> => [<<"personal">>, <<"organization">>]},
    run_checks([fun check_uuid/1, fun(B) -> check_scope(B, Allowed) end], Body, #{});
parse_recovery_context(_) ->
    {error, invalid_body}.

-spec run_checks([function()], map(), map()) -> {ok, map()} | {error, atom()}.
run_checks(Checks, Body, Acc) -> talk_record_capture_input:run_checks(Checks, Body, Acc).

-spec check_uuid(map()) -> {ok, map()} | {error, invalid_client_recording_id}.
check_uuid(Body) ->
    case maps:get(<<"client_recording_id">>, Body, <<>>) of
        Bin when is_binary(Bin) ->
            case is_uuid(Bin) of
                true -> {ok, #{client_recording_id => Bin}};
                false -> {error, invalid_client_recording_id}
            end;
        _ ->
            {error, invalid_client_recording_id}
    end.

-spec check_notice_ack(map()) -> {ok, map()} | {error, notice_ack_required}.
check_notice_ack(Body) ->
    case maps:get(<<"notice_ack">>, Body, false) of
        true -> {ok, #{}};
        _ -> {error, notice_ack_required}
    end.

-spec check_policy_version(map(), map()) -> {ok, map()} | {error, atom()}.
check_policy_version(Body, Cap) ->
    Current = maps:get(<<"policy_version">>, Cap, <<>>),
    case maps:get(<<"policy_version">>, Body, undefined) of
        V when is_binary(V), V =/= <<>> ->
            case V =:= Current of
                true -> {ok, #{policy_version => V}};
                false -> {error, policy_version_changed}
            end;
        _ ->
            {error, policy_version_missing}
    end.

-spec check_source_type(map()) -> {ok, map()} | {error, unsupported_source_type}.
check_source_type(Body) ->
    case maps:get(<<"source_type">>, Body, <<"live_recording">>) of
        <<"live_recording">> -> {ok, #{}};
        _ -> {error, unsupported_source_type}
    end.

%% @doc scope/归属组合（§8）：personal 拒绝 org/ws 字段；organization 必填
%% org；scope 必须在能力面 scopes 内（关闭能力不能仅 UI 禁用）。
-spec check_scope(map(), map()) -> {ok, map()} | {error, atom()}.
check_scope(Body, Cap) ->
    Enabled = [to_bin(S) || S <- maps:get(<<"scopes">>, Cap, [])],
    case maps:get(<<"scope">>, Body, <<"personal">>) of
        <<"personal">> = Scope ->
            scope_enabled_or_error(Scope, Enabled, fun() ->
                reject_org_fields(Body, #{scope => Scope})
            end);
        <<"organization">> = Scope ->
            scope_enabled_or_error(Scope, Enabled, fun() ->
                check_org_fields(Body, #{scope => Scope})
            end);
        _ ->
            {error, invalid_scope}
    end.

-spec scope_enabled_or_error(binary(), [binary()], fun()) -> {ok, map()} | {error, atom()}.
scope_enabled_or_error(Scope, Enabled, Cont) ->
    case lists:member(Scope, Enabled) of
        true -> Cont();
        false -> {error, scope_not_enabled}
    end.

-spec reject_org_fields(map(), map()) -> {ok, map()} | {error, personal_with_org}.
reject_org_fields(Body, Acc) ->
    case maps:is_key(<<"organization_id">>, Body) orelse maps:is_key(<<"workspace_id">>, Body) of
        false ->
            {ok, Acc};
        true ->
            {error, personal_with_org}
    end.

-spec check_org_fields(map(), map()) -> {ok, map()} | {error, atom()}.
check_org_fields(Body, Acc) ->
    case positive_int(maps:get(<<"organization_id">>, Body, 0)) of
        {ok, OrgId} ->
            check_org_ws(Body, Acc#{organization_id => OrgId});
        _ ->
            %% organization 记录必填 organization_id（§8 创建上下文表）
            {error, invalid_organization_id}
    end.

-spec check_org_ws(map(), map()) -> {ok, map()} | {error, atom()}.
check_org_ws(Body, Acc) ->
    case positive_int(maps:get(<<"workspace_id">>, Body, 0)) of
        {ok, WsId} ->
            {ok, Acc#{workspace_id => WsId}};
        error ->
            {error, invalid_workspace_id};
        {ok_zero} ->
            {ok, Acc#{workspace_id => 0}}
    end.

-spec check_text_fields(map()) -> {ok, map()} | {error, atom()}.
check_text_fields(Body) ->
    Title = maps:get(<<"title">>, Body, <<>>),
    Language = maps:get(<<"language">>, Body, <<>>),
    case {is_binary(Title), is_binary(Language)} of
        {true, true} ->
            check_lengths(Title, Language);
        {false, _} ->
            {error, invalid_title};
        _ ->
            {error, invalid_language}
    end.

-spec check_lengths(binary(), binary()) -> {ok, map()} | {error, atom()}.
check_lengths(Title, Language) ->
    case {string:length(Title) =< ?MAX_TITLE_CHARS, byte_size(Language) =< ?MAX_LANGUAGE_BYTES} of
        {true, true} ->
            {ok, #{title => Title, language => Language}};
        {false, _} ->
            {error, invalid_title};
        {_, false} ->
            {error, invalid_language}
    end.

-spec check_processing_mode(map()) -> {ok, map()} | {error, invalid_processing_mode}.
check_processing_mode(Body) ->
    case maps:get(<<"processing_mode">>, Body, <<"record_only">>) of
        M when M =:= <<"record_only">>; M =:= <<"transcribe">>; M =:= <<"summarize">> ->
            {ok, #{processing_mode => M}};
        _ ->
            {error, invalid_processing_mode}
    end.

%% ===================================================================
%% 编辑入参解析
%% ===================================================================

-define(IMMUTABLE_KEYS, [
    <<"owner_user_id">>,
    <<"owner">>,
    <<"created_by">>,
    <<"scope">>,
    <<"organization_id">>,
    <<"workspace_id">>,
    <<"client_recording_id">>
]).

%% @doc PATCH 白名单：title/language/notes；保留字段出现即 immutable_field；
%% notes 限长；至少一个可编辑字段。
-spec parse_update(map()) -> {ok, map()} | {error, atom()}.
parse_update(Body) when is_map(Body) ->
    case lists:any(fun(K) -> maps:is_key(K, Body) end, ?IMMUTABLE_KEYS) of
        true ->
            {error, immutable_field};
        false ->
            pick_update_fields(Body)
    end;
parse_update(_) ->
    {error, invalid_body}.

-spec pick_update_fields(map()) -> {ok, map()} | {error, atom()}.
pick_update_fields(Body) ->
    Picked = maps:filter(
        fun(_K, V) -> V =/= undefined end,
        maps:with([<<"title">>, <<"language">>, <<"notes">>], Body)
    ),
    case map_size(Picked) of
        0 ->
            {error, missing_editable_field};
        _ ->
            validate_update_fields(Picked)
    end.

-spec validate_update_fields(map()) -> {ok, map()} | {error, atom()}.
validate_update_fields(Picked) ->
    Checks = [
        {<<"title">>, fun check_update_title/1, invalid_title},
        {<<"language">>, fun check_update_language/1, invalid_language},
        {<<"notes">>, fun check_update_notes/1, invalid_notes}
    ],
    validate_fields(Checks, Picked, ok).

-spec validate_fields([{binary(), function(), atom()}], map(), ok | {error, atom()}) ->
    {ok, map()} | {error, atom()}.
validate_fields(_, _, {error, Reason}) ->
    {error, Reason};
validate_fields([], Picked, ok) ->
    {ok, Picked};
validate_fields([{Key, Check, Reason} | Rest], Picked, ok) ->
    case maps:find(Key, Picked) of
        {ok, Value} ->
            case Check(Value) of
                ok -> validate_fields(Rest, Picked, ok);
                error -> validate_fields(Rest, Picked, {error, Reason})
            end;
        error ->
            validate_fields(Rest, Picked, ok)
    end.

-spec check_update_title(term()) -> ok | error.
check_update_title(V) when is_binary(V) ->
    case string:length(V) =< ?MAX_TITLE_CHARS of
        true -> ok;
        false -> error
    end;
check_update_title(_) ->
    error.

-spec check_update_language(term()) -> ok | error.
check_update_language(V) when is_binary(V), byte_size(V) =< ?MAX_LANGUAGE_BYTES ->
    ok;
check_update_language(_) ->
    error.

-spec check_update_notes(term()) -> ok | error.
check_update_notes(V) when is_binary(V), byte_size(V) =< ?MAX_NOTES_BYTES ->
    ok;
check_update_notes(_) ->
    error.

%% ===================================================================
%% 列表筛选 / If-Match / 幂等键
%% ===================================================================

%% @doc 列表筛选归一化：filter ∈ all|owned|shared；org/ws 仅在
%% scope=organization 下允许（否则 422，防“默认看全企业”式放宽）；
%% q 限长 100。
-spec parse_filters(map()) -> {ok, map()} | {error, invalid_filter}.
parse_filters(Filters) when is_map(Filters) ->
    Scope = maps:get(<<"scope">>, Filters, <<>>),
    OrgId = maps:get(<<"organization_id">>, Filters, 0),
    WsId = maps:get(<<"workspace_id">>, Filters, 0),
    Q = maps:get(<<"q">>, Filters, <<>>),
    ViewFilter = maps:get(<<"filter">>, Filters, <<"all">>),
    Cond =
        valid_scope_param(Scope) andalso valid_view_filter(ViewFilter) andalso
            query_length_ok(Q) andalso scope_org_compatible(Scope, OrgId, WsId),
    case Cond of
        true ->
            {ok, #{
                scope => Scope,
                organization_id => default_zero(OrgId),
                workspace_id => default_zero(WsId),
                filter => ViewFilter,
                q => Q
            }};
        false ->
            {error, invalid_filter}
    end;
parse_filters(_) ->
    {error, invalid_filter}.

-spec valid_scope_param(binary()) -> boolean().
valid_scope_param(S) ->
    S =:= <<>> orelse S =:= <<"personal">> orelse S =:= <<"organization">>.

-spec valid_view_filter(binary()) -> boolean().
valid_view_filter(F) ->
    F =:= <<"all">> orelse F =:= <<"owned">> orelse F =:= <<"shared">>.

-spec scope_org_compatible(binary(), term(), term()) -> boolean().
scope_org_compatible(<<"organization">>, OrgId, WsId) ->
    nonneg_int(OrgId) andalso nonneg_int(WsId) andalso (WsId =:= 0 orelse OrgId =/= 0);
scope_org_compatible(<<>>, OrgId, WsId) ->
    nonneg_int(OrgId) andalso nonneg_int(WsId);
scope_org_compatible(<<"personal">>, OrgId, WsId) ->
    OrgId =:= 0 andalso WsId =:= 0;
scope_org_compatible(_, _, _) ->
    false.

-spec query_length_ok(term()) -> boolean().
query_length_ok(Q) when is_binary(Q) ->
    string:length(Q) =< ?MAX_QUERY_CHARS;
query_length_ok(Q) ->
    Q =:= undefined orelse Q =:= 0.

-spec default_zero(term()) -> integer().
default_zero(V) ->
    case positive_int(V) of
        {ok, N} -> N;
        _ -> 0
    end.

%% @doc limit 钳制 20..100（默认 20；越界取边界，不 422——列表友好性）。
-spec parse_limit(term()) -> pos_integer().
parse_limit(V) ->
    N =
        case V of
            Bin when is_binary(Bin) -> safe_to_integer(Bin, 20);
            Int when is_integer(Int) -> Int;
            _ -> 20
        end,
    max(20, min(100, N)).

-spec safe_to_integer(binary(), integer()) -> integer().
safe_to_integer(Bin, Default) ->
    try
        binary_to_integer(Bin)
    catch
        _:_ -> Default
    end.

%% @doc If-Match（§3：缺头 428；非法形态 422；值应为当前 ETag `"N"` 或 N）。
-spec parse_if_match(binary() | undefined) -> {ok, pos_integer()} | {error, atom()}.
parse_if_match(undefined) ->
    {error, precondition_required};
parse_if_match(Bin) when is_binary(Bin) ->
    StripQuotes = binary:replace(
        binary:replace(Bin, <<"\"">>, <<>>, [global]),
        <<"W/">>,
        <<>>,
        [global]
    ),
    try
        case binary_to_integer(StripQuotes) of
            Rev when Rev >= 1 -> {ok, Rev};
            _ -> {error, invalid_if_match}
        end
    catch
        _:_ -> {error, invalid_if_match}
    end.

%% @doc Idempotency-Key 形态（对齐 enterprise_internal_idempotency:valid_key：
%% 1..128 可打印 ASCII）。
-spec valid_idempotency_key(binary() | undefined) -> boolean().
valid_idempotency_key(Key) when is_binary(Key) ->
    byte_size(Key) >= 1 andalso byte_size(Key) =< 128 andalso is_printable_ascii(Key);
valid_idempotency_key(_) ->
    false.

%% @doc 创建请求规范化指纹（freeze §2.5 公式）。
-spec request_digest(map()) -> {ok, binary()} | {error, non_canonical}.
request_digest(Body) when is_map(Body) ->
    request_digest(<<"POST">>, ?CREATE_PATH, Body);
request_digest(_) ->
    {error, non_canonical}.

%% @doc 通用请求规范化指纹（T07 采集域按资源路径调用）。
-spec request_digest(binary(), binary(), map()) -> {ok, binary()} | {error, non_canonical}.
request_digest(Method, Path, Body) when is_map(Body), is_binary(Method), is_binary(Path) ->
    case enterprise_cursor_v2:canonical_json(Body) of
        {ok, Canonical} ->
            {ok,
                binary:encode_hex(
                    crypto:hash(sha256, <<Method/binary, Path/binary, "\n", Canonical/binary>>),
                    lowercase
                )};
        {error, non_canonical} ->
            {error, non_canonical}
    end;
request_digest(_, _, _) ->
    {error, non_canonical}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec is_uuid(binary()) -> boolean().
is_uuid(Bin) -> talk_record_capture_input:is_uuid(Bin).

-spec positive_int(term()) -> {ok, pos_integer()} | {ok_zero} | error.
positive_int(N) when is_integer(N), N > 0 -> {ok, N};
positive_int(N) when is_binary(N) ->
    try
        positive_int(binary_to_integer(N))
    catch
        _:_ -> positive_int_zero_or_error(N)
    end;
positive_int(0) ->
    {ok_zero};
positive_int(<<"0">>) ->
    {ok_zero};
positive_int(null) ->
    {ok_zero};
positive_int(undefined) ->
    {ok_zero};
positive_int(_) ->
    error.

-spec positive_int_zero_or_error(term()) -> {ok_zero} | error.
positive_int_zero_or_error(<<"0">>) -> {ok_zero};
positive_int_zero_or_error(null) -> {ok_zero};
positive_int_zero_or_error(undefined) -> {ok_zero};
positive_int_zero_or_error(_) -> error.

-spec nonneg_int(term()) -> boolean().
nonneg_int(N) when is_integer(N), N >= 0 -> true;
nonneg_int(N) when is_binary(N) ->
    try
        binary_to_integer(N) >= 0
    catch
        _:_ -> false
    end;
nonneg_int(undefined) ->
    true;
nonneg_int(null) ->
    true;
nonneg_int(_) ->
    false.

-spec to_bin(atom() | binary()) -> binary().
to_bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_bin(B) when is_binary(B) ->
    B.

-spec is_printable_ascii(binary()) -> boolean().
is_printable_ascii(Bin) -> talk_record_capture_input:is_printable_ascii(Bin).

%% Capture input remains available through the original public facade.
-spec parse_capture_create(map()) -> {ok, map()} | {error, atom()}.
parse_capture_create(Body) -> talk_record_capture_input:parse_capture_create(Body).
-spec parse_event(map()) -> {ok, map()} | {error, atom()}.
parse_event(Body) -> talk_record_capture_input:parse_event(Body).
-spec parse_audio_part(map()) -> {ok, map()} | {error, atom()}.
parse_audio_part(Body) -> talk_record_capture_input:parse_audio_part(Body).
-spec parse_confirm_receipt(map()) -> {ok, map()} | {error, atom()}.
parse_confirm_receipt(Body) -> talk_record_capture_input:parse_confirm_receipt(Body).
-spec parse_device_session(map()) -> {ok, map()} | {error, atom()}.
parse_device_session(Body) -> talk_record_capture_input:parse_device_session(Body).
-spec parse_abandon(map()) -> {ok, map()} | {error, atom()}.
parse_abandon(Body) -> talk_record_capture_input:parse_abandon(Body).
-spec valid_events() -> [binary()].
valid_events() -> talk_record_capture_input:valid_events().
