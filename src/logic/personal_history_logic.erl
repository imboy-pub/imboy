-module(personal_history_logic).
-moduledoc "个人消息历史域 Logic 层（MH-01 合同 v1.1 / MH-02）。".
%%%
%%% personal_history_logic —— 个人消息历史域 Logic 层（MH-01 合同 v1.1 / MH-02）
%%%
%%% 业务规则与合同错误码映射（§2.7 fail-closed：全部显式错误码，无静默吞）：
%%%   403 not_enabled / not_participant / identity_rejected（认证面在中间件）
%%%   409 generation_stale / purge_in_progress / version_conflict / nonce_reused
%%%   410 generation_expired / page_token_expired / rescan_bound_exceeded
%%%   422 key_package_missing / malformed_package / malformed_request
%%%   503 transient_failure（客户端安全重试）
%%% 服务端零解密（§0）：只透传版本化密文与用途独立密钥包，本模块零密码学。
%%%

-export([status/1]).
-export([opt_in/2]).
-export([upload/2]).
-export([changes/2]).
-export([ack_cursor/2]).
-export([page/2, account_snapshot/2]).
-export([put_key_package/2]).
-export([get_key_package/2]).
-export([disable/1]).
-export([delete_status/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% KDF 迭代下限（§10.4 钉 ≥310000，与 279 迁移注释对齐）
-define(MIN_KDF_ITERATIONS, 310000).
%% 单条密文与封装密钥上限（1MB）
-define(MAX_BLOB_BYTES, 1048576).
%% msg_id 上限（对齐 277 varchar(40)）
-define(MAX_MSG_ID_BYTES, 40).

%% ===================================================================
%% 2.1 status
%% ===================================================================

%% @doc 读取本人授权状态；无行 = 默认 OFF（generation 0，MSG-H01 零写入）。
-spec status(integer()) -> {ok, map()} | {error, binary(), integer()}.
status(AccountId) ->
    Domain = personal_history_ds:domain(),
    case personal_history_repo:find_authorization(AccountId, Domain) of
        {ok, undefined} ->
            {ok, #{<<"state">> => <<"OFF">>, <<"generation">> => 0, <<"domain">> => Domain}};
        {ok, Row} ->
            {ok, status_payload(AccountId, Domain, Row)};
        {error, _Reason} ->
            {error, <<"transient_failure">>, 503}
    end.

status_payload(AccountId, Domain, Row) ->
    Base = #{
        <<"state">> => maps:get(<<"state">>, Row),
        <<"domain">> => Domain,
        <<"generation">> => maps:get(<<"generation">>, Row)
    },
    Optional =
        case maps:get(<<"enabled_at">>, Row, null) of
            null -> Base;
            EnabledAt -> Base#{<<"enabled_at">> => EnabledAt}
        end,
    case personal_history_repo:max_event_seq(AccountId, Domain) of
        {ok, MaxSeq} -> Optional#{<<"upload_cursor">> => MaxSeq};
        {error, _} -> Optional
    end.

%% ===================================================================
%% 2.2 opt-in
%% ===================================================================

%% @doc 开启本人云端副本（幂等：同代次重复返回同状态；purge 中 409）。
%% 新代次不回填（V1 自开启时刻起向前同步）。
-spec opt_in(integer(), binary()) -> {ok, map()} | {error, binary(), integer()}.
opt_in(AccountId, ClientInstanceId) ->
    Domain = personal_history_ds:domain(),
    case personal_history_ds:opt_in(AccountId, Domain, ClientInstanceId) of
        {ok, Row, Kind} ->
            {ok, #{
                <<"state">> => maps:get(<<"state">>, Row),
                <<"generation">> => maps:get(<<"generation">>, Row),
                <<"enabled_at">> => maps:get(<<"enabled_at">>, Row),
                <<"result">> => atom_to_binary(Kind, utf8)
            }};
        {error, purge_in_progress} ->
            {error, <<"purge_in_progress">>, 409};
        {error, _Reason} ->
            {error, <<"generation_stale">>, 409}
    end.

%% ===================================================================
%% 2.3 upload
%% ===================================================================

%% @doc 上传一条个人历史密文（幂等键 (account,domain,conv,msg_id)）。
%% Params: conversation_id, msg_id, action_version, conv_seq, ciphertext(b64),
%% envelope_meta{alg,key_version,nonce(b64),aad}, client_ts, generation(可选).
-spec upload(integer(), map()) -> {ok, map()} | {error, binary(), integer()}.
upload(AccountId, Params) ->
    Domain = personal_history_ds:domain(),
    case upload_item(Params) of
        {error, Code} ->
            {error, Code, 422};
        {ok, Item} ->
            upload_guard_participation(AccountId, Domain, Item)
    end.

upload_guard_participation(AccountId, Domain, Item) ->
    RawConv = maps:get(conversation_id, Item),
    case personal_history_ds:canonical_conversation(RawConv) of
        {error, bad_conversation} ->
            %% 域外/畸形会话键与未参与同型同时序（§10.5 防枚举 oracle）
            {error, <<"not_participant">>, 403};
        {ok, Canonical} ->
            case personal_history_ds:is_participant(AccountId, Canonical) of
                false -> {error, <<"not_participant">>, 403};
                true -> do_upload(AccountId, Domain, Item#{conversation_id := Canonical})
            end
    end.

do_upload(AccountId, Domain, Item) ->
    case personal_history_ds:upload(AccountId, Domain, Item) of
        {ok, Kind, Row, Generation} ->
            {ok, #{
                <<"result">> => atom_to_binary(Kind, utf8),
                <<"conversation_id">> => maps:get(conversation_id, Item),
                <<"msg_id">> => maps:get(msg_id, Item),
                <<"action_version">> => maps:get(<<"action_version">>, Row),
                <<"conv_seq">> => maps:get(<<"conv_seq">>, Row),
                <<"event_seq">> => maps:get(<<"event_seq">>, Row),
                <<"generation">> => Generation
            }};
        {error, Reason} ->
            upload_error(Reason)
    end.

upload_error(not_enabled) -> {error, <<"not_enabled">>, 403};
upload_error(generation_expired) -> {error, <<"generation_expired">>, 410};
upload_error(nonce_reused) -> {error, <<"nonce_reused">>, 409};
upload_error(version_conflict) -> {error, <<"version_conflict">>, 409};
upload_error(backfill_evidence_missing) -> {error, <<"backfill_evidence_missing">>, 422};
upload_error(key_package_missing) -> {error, <<"key_package_missing">>, 422};
upload_error(_Reason) -> {error, <<"transient_failure">>, 503}.

%% @doc 组装并校验上传项（422 malformed_request 类显式拒绝）。
upload_item(Params) ->
    Meta = maps:get(<<"envelope_meta">>, Params, #{}),
    Shape = #{
        conv_id => maps:get(<<"conversation_id">>, Params, <<>>),
        msg_id => maps:get(<<"msg_id">>, Params, <<>>),
        action_version => maps:get(<<"action_version">>, Params, 0),
        conv_seq => maps:get(<<"conv_seq">>, Params, -1),
        generation => maps:get(<<"generation">>, Params, undefined),
        key_version => maps:get(<<"key_version">>, Meta, 0),
        algo => maps:get(<<"alg">>, Meta, <<>>),
        nonce_b64 => maps:get(<<"nonce">>, Meta, <<>>),
        aad => maps:get(<<"aad">>, Meta, <<>>),
        ciphertext_b64 => maps:get(<<"ciphertext">>, Params, <<>>),
        meta => Meta
    },
    case valid_upload_shape(Shape) of
        false -> {error, <<"malformed_request">>};
        true -> decode_upload_blobs(Shape)
    end.

valid_upload_shape(#{
    conv_id := ConvId,
    msg_id := MsgId,
    action_version := AV,
    conv_seq := Seq,
    generation := Gen,
    key_version := KV,
    algo := Algo
}) ->
    is_binary(ConvId) andalso byte_size(ConvId) > 0 andalso
        is_binary(MsgId) andalso byte_size(MsgId) > 0 andalso
        byte_size(MsgId) =< ?MAX_MSG_ID_BYTES andalso
        is_integer(AV) andalso AV >= 0 andalso
        is_integer(Seq) andalso Seq >= 0 andalso
        (Gen =:= undefined orelse (is_integer(Gen) andalso Gen >= 0)) andalso
        is_integer(KV) andalso KV >= 1 andalso
        is_binary(Algo) andalso byte_size(Algo) > 0.

decode_upload_blobs(#{nonce_b64 := NonceB64, ciphertext_b64 := CiphertextB64} = Shape) ->
    case {decode_b64(NonceB64), decode_b64(CiphertextB64)} of
        {{ok, Nonce}, {ok, Ciphertext}} when
            byte_size(Nonce) > 0,
            byte_size(Ciphertext) > 0,
            byte_size(Ciphertext) =< ?MAX_BLOB_BYTES
        ->
            {ok, upload_item_from(Shape#{nonce => Nonce, ciphertext => Ciphertext})};
        _ ->
            {error, <<"malformed_request">>}
    end.

upload_item_from(
    #{
        conv_id := ConvId,
        msg_id := MsgId,
        action_version := AV,
        conv_seq := Seq,
        generation := Gen,
        key_version := KV,
        nonce := Nonce,
        ciphertext := Ciphertext,
        aad := Aad,
        meta := Meta
    }
) ->
    #{
        conversation_id => ConvId,
        msg_id => MsgId,
        action_version => AV,
        conv_seq => Seq,
        generation => Gen,
        key_version => KV,
        nonce => Nonce,
        ciphertext => Ciphertext,
        envelope_meta => Meta#{<<"aad">> => Aad}
    }.

%% ===================================================================
%% 2.4 changes / ack-cursor / page
%% ===================================================================

%% @doc 拉取变更事件（cursor 单调；窗口外 reset_hint 触发有界重扫）。
%% Opts: cursor(integer), limit(integer), client_instance(binary)
-spec changes(integer(), map()) -> {ok, map()} | {error, binary(), integer()}.
changes(AccountId, #{snapshot := account} = Opts) ->
    case valid_snapshot_instance(maps:get(client_instance, Opts, <<>>)) of
        true -> do_changes(AccountId, Opts, fun snapshot_event/1);
        false -> read_error(malformed_request)
    end;
changes(AccountId, Opts) ->
    do_changes(AccountId, Opts, fun change_event/1).

do_changes(AccountId, Opts, FormatEvent) ->
    Domain = personal_history_ds:domain(),
    Cursor = maps:get(cursor, Opts, 0),
    Limit0 = maps:get(limit, Opts, 50),
    Limit = clamp_limit(Limit0),
    ClientInstance = maps:get(client_instance, Opts, <<>>),
    case personal_history_ds:issue_changes(AccountId, Domain, ClientInstance, Cursor, Limit) of
        {ok, Generation, Events, NextCursor, ResetHint} ->
            WireCursor =
                case NextCursor of
                    undefined -> null;
                    _ -> NextCursor
                end,
            {ok, #{
                <<"generation">> => Generation,
                <<"events">> => lists:map(FormatEvent, Events),
                <<"next_cursor">> => WireCursor,
                <<"reset_hint">> => ResetHint
            }};
        {error, Reason} ->
            read_error(Reason)
    end.

change_event(Row) ->
    (archive_wire_item(Row))#{
        <<"conversation_id">> => maps:get(<<"conversation_id">>, Row),
        <<"event_seq">> => maps:get(<<"event_seq">>, Row)
    }.

%% @doc durably 确认游标（幂等；仅推进 ≤ 已发放最大值）。
-spec ack_cursor(integer(), map()) -> {ok, map()} | {error, binary(), integer()}.
ack_cursor(AccountId, #{completion_token := Token} = Opts) ->
    confirm_account_snapshot(AccountId, Opts, Token);
ack_cursor(AccountId, Opts) ->
    Domain = personal_history_ds:domain(),
    Cursor = maps:get(cursor, Opts, -1),
    Generation = maps:get(generation, Opts, undefined),
    ClientInstance = maps:get(client_instance, Opts, <<>>),
    ValidGeneration =
        Generation =:= undefined orelse
            (is_integer(Generation) andalso Generation > 0),
    case is_integer(Cursor) andalso Cursor >= 0 andalso ValidGeneration of
        false ->
            {error, <<"malformed_request">>, 422};
        true ->
            do_ack_cursor(AccountId, Domain, ClientInstance, Cursor, Generation)
    end.

do_ack_cursor(AccountId, Domain, ClientInstance, Cursor, ExpectedGeneration) ->
    ack_result(
        personal_history_ds:ack_cursor(
            AccountId, Domain, ClientInstance, Cursor, ExpectedGeneration
        )
    ).

confirm_account_snapshot(AccountId, Opts, Token) ->
    Cursor = maps:get(cursor, Opts, -1),
    Generation = maps:get(generation, Opts, undefined),
    Instance = maps:get(client_instance, Opts, <<>>),
    case
        is_integer(Cursor) andalso Cursor >= 0 andalso
            is_integer(Generation) andalso Generation > 0 andalso
            valid_snapshot_instance(Instance) andalso is_binary(Token) andalso byte_size(Token) > 0
    of
        true ->
            ack_result(
                personal_history_snapshot_ds:confirm_snapshot(
                    AccountId, personal_history_ds:domain(), Instance, Cursor, Generation, Token
                )
            );
        false ->
            read_error(malformed_request)
    end.

ack_result(Result) ->
    case Result of
        {ok, Generation, Accepted, Acked, Issued} ->
            {ok, #{
                <<"accepted">> => Accepted,
                <<"advanced_to">> => Acked,
                <<"issued_cursor">> => Issued,
                <<"generation">> => Generation
            }};
        {error, Reason} ->
            read_error(Reason)
    end.

-spec account_snapshot(integer(), map()) -> {ok, map()} | {error, binary(), integer()}.
account_snapshot(AccountId, #{client_instance := Instance, page_token := Token}) ->
    case valid_snapshot_instance(Instance) of
        true ->
            case
                personal_history_snapshot_ds:account_snapshot(
                    AccountId, personal_history_ds:domain(), Instance, Token
                )
            of
                {ok, Generation, Events, Position} ->
                    {ok, (snapshot_position(Position))#{
                        <<"generation">> => Generation,
                        <<"events">> => [snapshot_event(Row) || Row <- Events]
                    }};
                {error, Reason} ->
                    read_error(Reason)
            end;
        false ->
            read_error(malformed_request)
    end.

snapshot_event(#{<<"deleted_at">> := null} = Row) ->
    case decode_jsonb(maps:get(<<"accepted_message">>, Row, null)) of
        Fact when is_map(Fact) -> change_event(Row);
        _ ->
            Identity = maps:with(
                [
                    <<"conversation_id">>,
                    <<"msg_id">>,
                    <<"action_version">>,
                    <<"conv_seq">>,
                    <<"event_seq">>
                ],
                Row
            ),
            Identity#{<<"deleted">> => false, <<"suppressed">> => true}
    end;
snapshot_event(Row) ->
    change_event(Row).

snapshot_position({more, Token}) ->
    #{<<"has_more">> => true, <<"page_token">> => Token};
snapshot_position({complete, Watermark, Token}) ->
    #{
        <<"has_more">> => false,
        <<"page_token">> => null,
        <<"recovery_cursor">> => Watermark,
        <<"completion_token">> => Token
    }.

valid_snapshot_instance(Instance) ->
    is_binary(Instance) andalso byte_size(Instance) > 0 andalso byte_size(Instance) =< 128.

%% @doc 会话分页快照（conv_seq DESC；page_token 过期 410 触发有界重扫）。
%% Opts: conversation_id(binary), before(integer), page_token(binary)
-spec page(integer(), map()) -> {ok, map()} | {error, binary(), integer()}.
page(AccountId, Opts) ->
    Domain = personal_history_ds:domain(),
    ConvId = maps:get(conversation_id, Opts, <<>>),
    Before = maps:get(before, Opts, 0),
    Token = maps:get(page_token, Opts, undefined),
    case
        is_binary(ConvId) andalso byte_size(ConvId) > 0 andalso
            is_integer(Before) andalso Before >= 0 andalso
            (Token =:= undefined orelse is_binary(Token))
    of
        false ->
            {error, <<"malformed_request">>, 422};
        true ->
            do_page(AccountId, Domain, ConvId, Before, Token)
    end.

do_page(AccountId, Domain, ConvId, Before, Token) ->
    case personal_history_ds:canonical_conversation(ConvId) of
        {error, bad_conversation} ->
            {error, <<"not_participant">>, 403};
        {ok, Canonical} ->
            case personal_history_ds:is_participant(AccountId, Canonical) of
                false -> {error, <<"not_participant">>, 403};
                true -> do_page_fetch(AccountId, Domain, Canonical, Before, Token)
            end
    end.

do_page_fetch(AccountId, Domain, Canonical, Before, Token) ->
    case personal_history_ds:page(AccountId, Domain, Canonical, Before, Token) of
        {ok, Generation, Rows, NextToken, HasMore} ->
            {ok, #{
                <<"generation">> => Generation,
                <<"conversation_id">> => Canonical,
                <<"messages">> => [page_item(Row) || Row <- Rows],
                <<"page_token">> => NextToken,
                <<"has_more">> => HasMore
            }};
        {error, Reason} ->
            read_error(Reason)
    end.

page_item(Row) ->
    (archive_wire_item(Row))#{<<"created_at">> => maps:get(<<"created_at">>, Row)}.

archive_wire_item(Row) ->
    Identity = #{
        <<"msg_id">> => maps:get(<<"msg_id">>, Row),
        <<"action_version">> => maps:get(<<"action_version">>, Row),
        <<"conv_seq">> => maps:get(<<"conv_seq">>, Row),
        <<"key_version">> => maps:get(<<"key_version">>, Row)
    },
    case maps:get(<<"deleted_at">>, Row) of
        null ->
            with_accepted_message(Row, Identity#{
                <<"ciphertext">> => base64:encode(maps:get(<<"ciphertext">>, Row)),
                <<"envelope_meta">> => decode_jsonb(maps:get(<<"envelope_meta">>, Row)),
                <<"deleted">> => false
            });
        _ ->
            Identity#{<<"deleted">> => true}
    end.

with_accepted_message(Row, Item) ->
    case decode_jsonb(maps:get(<<"accepted_message">>, Row, null)) of
        Fact when is_map(Fact) -> Item#{<<"accepted_message">> => Fact};
        _ -> Item
    end.

read_error(malformed_request) -> {error, <<"malformed_request">>, 422};
read_error(not_enabled) -> {error, <<"not_enabled">>, 403};
read_error(generation_expired) -> {error, <<"generation_expired">>, 410};
read_error(page_token_expired) -> {error, <<"page_token_expired">>, 410};
read_error(rescan_bound_exceeded) -> {error, <<"rescan_bound_exceeded">>, 410};
read_error(_Reason) -> {error, <<"transient_failure">>, 503}.

%% ===================================================================
%% 2.5 key-package
%% ===================================================================

%% @doc 存用途独立密钥包（与投递 E2EE 密钥树隔离，§4）。
%% kdf_meta 迭代下限 ≥310000；PEM 明文拒收（§10.5 下限沿用）。
-spec put_key_package(integer(), map()) -> {ok, map()} | {error, binary(), integer()}.
put_key_package(AccountId, Params) ->
    Domain = personal_history_ds:domain(),
    Expected = maps:get(<<"generation">>, Params, undefined),
    case key_package_item(Params) of
        {ok, {KeyVersion, Pkg}} when Expected =:= undefined; is_integer(Expected), Expected > 0 ->
            put_key_package_authorized(AccountId, Domain, KeyVersion, Pkg, Expected);
        _ ->
            {error, <<"malformed_package">>, 422}
    end.

put_key_package_authorized(AccountId, Domain, KeyVersion, Pkg, Expected) ->
    case personal_history_repo:find_authorization(AccountId, Domain) of
        {ok, #{<<"state">> := <<"ON">>, <<"generation">> := G}} when
            Expected =:= undefined; Expected =:= G
        ->
            save_key_package(AccountId, Domain, KeyVersion, G, Pkg);
        {ok, #{<<"state">> := State}} when
            State =:= <<"ON">>; State =:= <<"OFF_PURGE_PENDING">>; State =:= <<"OFF_PURGED">>
        ->
            {error, <<"generation_expired">>, 410};
        {ok, _} ->
            {error, <<"not_enabled">>, 403};
        {error, _Reason} ->
            {error, <<"transient_failure">>, 503}
    end.

save_key_package(AccountId, Domain, KeyVersion, G, Pkg) ->
    case personal_history_ds:put_key_package(AccountId, Domain, KeyVersion, G, Pkg) of
        {ok, Row} ->
            {ok, #{
                <<"key_version">> => KeyVersion,
                <<"generation">> => G,
                <<"created_at">> => maps:get(<<"created_at">>, Row)
            }};
        {error, version_conflict} ->
            {error, <<"version_conflict">>, 409};
        {error, Reason} ->
            key_package_error(Reason)
    end.

key_package_error(not_enabled) -> {error, <<"not_enabled">>, 403};
key_package_error(generation_expired) -> {error, <<"generation_expired">>, 410};
key_package_error(_Reason) -> {error, <<"transient_failure">>, 503}.

key_package_item(Params) ->
    KeyVersion = maps:get(<<"key_version">>, Params, 0),
    WrappedB64 = maps:get(<<"wrapped_key">>, Params, <<>>),
    KdfMeta = maps:get(<<"kdf_meta">>, Params, #{}),
    IntegrityHash = maps:get(<<"integrity_hash">>, Params, <<>>),
    case
        is_integer(KeyVersion) andalso KeyVersion >= 1 andalso
            is_binary(IntegrityHash) andalso byte_size(IntegrityHash) > 0 andalso
            valid_key_package_kdf(KdfMeta)
    of
        false ->
            {error, <<"malformed_package">>};
        true ->
            decode_wrapped_key(WrappedB64, KdfMeta, IntegrityHash, KeyVersion)
    end.

valid_key_package_kdf(#{<<"iterations">> := Iterations} = Meta) when
    is_integer(Iterations), Iterations >= ?MIN_KDF_ITERATIONS
->
    Algorithm = maps:get(<<"alg">>, Meta, maps:get(<<"algorithm">>, Meta, <<>>)),
    is_binary(Algorithm) andalso byte_size(Algorithm) > 0 andalso
        maps:get(<<"algorithm">>, Meta, Algorithm) =:= Algorithm;
valid_key_package_kdf(_) ->
    false.

%% ===================================================================
%% 2.5b GET key-package（mh04-key-package-read-addendum：恢复读取入口）
%% ===================================================================

%% JS 安全整数上界（与 276-279 DDL / addendum 同域）。
-define(PH_SAFE_INT_MAX, 9007199254740991).

%% @doc 读取已封装密钥包：参数 → 授权（ON + 当前代次）→ 行级读取。
%%
%% 校验顺序（addendum）：参数无效 422 malformed_package_request →
%% 未开启 403 not_enabled → 代次失效 410 generation_expired →
%% 指定包缺失 422 key_package_missing → 暂时无法读取 503 transient_failure。
%% retired_at 不自动否定历史旧版本可读性（是否已销毁按当前授权及删账判定）。
-spec get_key_package(integer(), map()) -> {ok, map()} | {error, binary(), integer()}.
get_key_package(AccountId, Params) ->
    Domain = personal_history_ds:domain(),
    case key_package_read_params(Params) of
        {error, Code} ->
            {error, Code, 422};
        {ok, {KeyVersion, Generation}} ->
            get_key_package_authorized(AccountId, Domain, KeyVersion, Generation)
    end.

get_key_package_authorized(AccountId, Domain, KeyVersion, Generation) ->
    case personal_history_repo:find_authorization(AccountId, Domain) of
        {ok, #{<<"state">> := <<"ON">>, <<"generation">> := G}} ->
            case G =:= Generation of
                false ->
                    %% 代次比对当前 ON 授权：旧代次读一律 410（防复活/跨代读）。
                    {error, <<"generation_expired">>, 410};
                true ->
                    read_key_package(AccountId, Domain, KeyVersion, G)
            end;
        {ok, #{<<"state">> := State}} when
            State =:= <<"OFF_PURGE_PENDING">>; State =:= <<"OFF_PURGED">>
        ->
            {error, <<"generation_expired">>, 410};
        {ok, _} ->
            {error, <<"not_enabled">>, 403};
        {error, _Reason} ->
            {error, <<"transient_failure">>, 503}
    end.

read_key_package(AccountId, Domain, KeyVersion, Generation) ->
    case
        personal_history_repo:find_key_package_for_generation(
            AccountId, Domain, KeyVersion, Generation
        )
    of
        {ok, undefined} ->
            {error, <<"key_package_missing">>, 422};
        {ok, Row} ->
            {ok, #{
                <<"key_version">> => maps:get(<<"key_version">>, Row),
                <<"generation">> => maps:get(<<"generation">>, Row),
                <<"wrapped_key">> => base64:encode(maps:get(<<"wrapped_key">>, Row)),
                <<"kdf_meta">> => decode_jsonb(maps:get(<<"kdf_meta">>, Row)),
                <<"integrity_hash">> => maps:get(<<"integrity_hash">>, Row),
                <<"created_at">> => maps:get(<<"created_at">>, Row)
            }};
        {error, _Reason} ->
            {error, <<"transient_failure">>, 503}
    end.

%% @doc 查询参数恰好一份 key_version 与一份 generation，十进制 ASCII 数字
%% （无符号、无前导零、无指数/浮点）；generation 0..?PH_SAFE_INT_MAX、
%% version 1..?PH_SAFE_INT_MAX。重复份数由 HTTP 层（parse_qs 计数）裁决。
key_package_read_params(Params) ->
    KeyVersionRaw = maps:get(key_version, Params, undefined),
    GenerationRaw = maps:get(generation, Params, undefined),
    case {decimal_domain_int(KeyVersionRaw, 1), decimal_domain_int(GenerationRaw, 0)} of
        {{ok, KeyVersion}, {ok, Generation}} ->
            {ok, {KeyVersion, Generation}};
        _ ->
            {error, <<"malformed_package_request">>}
    end.

decimal_domain_int(Raw, Min) when is_binary(Raw), byte_size(Raw) > 0 ->
    case re:run(Raw, <<"^(0|[1-9][0-9]*)$">>, [{capture, none}]) of
        match ->
            try binary_to_integer(Raw) of
                Value when Value >= Min, Value =< ?PH_SAFE_INT_MAX ->
                    {ok, Value};
                _ ->
                    {error, out_of_domain}
            catch
                _:_ -> {error, out_of_domain}
            end;
        _ ->
            {error, malformed}
    end;
decimal_domain_int(_, _) ->
    {error, malformed}.

decode_wrapped_key(WrappedB64, KdfMeta, IntegrityHash, KeyVersion) ->
    case decode_b64(WrappedB64) of
        {ok, <<"-----BEGIN", _/binary>>} ->
            %% PEM 明文拒收（§10.5：279 沿用拒收下限）
            {error, <<"malformed_package">>};
        {ok, Wrapped} when
            byte_size(Wrapped) > 0, byte_size(Wrapped) =< ?MAX_BLOB_BYTES
        ->
            {ok,
                {KeyVersion, #{
                    wrapped_key => Wrapped,
                    kdf_meta => KdfMeta,
                    integrity_hash => IntegrityHash
                }}};
        _ ->
            {error, <<"malformed_package">>}
    end.

%% ===================================================================
%% 2.6 disable / delete-status
%% ===================================================================

%% @doc 关闭并删除本人云端历史：CAS generation++ → OFF_PURGE_PENDING（202），
%% 随后在线清理（W3 可重入）。旧代次上传/页令牌此后全部 410（W4）。
-spec disable(integer()) -> {ok, map()} | {error, binary(), integer()}.
disable(AccountId) ->
    Domain = personal_history_ds:domain(),
    case personal_history_ds:disable(AccountId, Domain) of
        {ok, _Row, Generation} ->
            Ticket = purge_ticket(AccountId, Generation),
            Payload = #{
                <<"state">> => <<"OFF_PURGE_PENDING">>,
                <<"purge_ticket">> => Ticket,
                <<"generation">> => Generation
            },
            %% 在线清理段独立事务（W3：崩在中途可整段重放；授权代次已先落账）
            case personal_history_ds:purge_online(AccountId, Domain, Generation) of
                {ok, _} ->
                    {ok, Payload};
                {error, _Reason} ->
                    {ok, Payload#{<<"phase">> => <<"pending">>}}
            end;
        {error, not_enabled} ->
            {error, <<"not_enabled">>, 403};
        {error, _Reason} ->
            {error, <<"transient_failure">>, 503}
    end.

purge_ticket(AccountId, Generation) ->
    %% 追踪票据（幂等重入同代次；非 DB 主键，账号+代次内唯一即可）
    Unique = erlang:unique_integer([positive]),
    <<"purge_", (integer_to_binary(AccountId))/binary, "_", (integer_to_binary(Generation))/binary,
        "_", (integer_to_binary(Unique))/binary>>.

%% @doc 清理进度：pending / online_done（备份到期为运维侧提示，§10.6 诚实化：
%% 逻辑删除非密码学擦除，PITR/备份窗口内存在回卷可能）。
-spec delete_status(integer()) -> {ok, map()} | {error, binary(), integer()}.
delete_status(AccountId) ->
    Domain = personal_history_ds:domain(),
    case personal_history_repo:find_authorization(AccountId, Domain) of
        {ok, undefined} ->
            {ok, #{<<"phase">> => <<"idle">>, <<"state">> => <<"OFF">>, <<"failures">> => []}};
        {ok, Row} ->
            {ok, delete_status_payload(Row)};
        {error, _Reason} ->
            {error, <<"transient_failure">>, 503}
    end.

delete_status_payload(#{<<"state">> := <<"OFF_PURGE_PENDING">>} = Row) ->
    #{
        <<"phase">> => <<"pending">>,
        <<"state">> => <<"OFF_PURGE_PENDING">>,
        <<"generation">> => maps:get(<<"generation">>, Row),
        <<"failures">> => [],
        <<"retry">> => true
    };
delete_status_payload(#{<<"state">> := <<"OFF_PURGED">>} = Row) ->
    #{
        <<"phase">> => <<"online_done">>,
        <<"state">> => <<"OFF_PURGED">>,
        <<"generation">> => maps:get(<<"generation">>, Row),
        <<"purged_at">> => maps:get(<<"purge_finished_at">>, Row),
        <<"failures">> => [],
        %% §10.6：purge=逻辑删除+封装销毁，非密码学擦除；窗口内读闸门关闭
        <<"backup_window_hint">> => <<"logical_delete_pitr_window_open">>
    };
delete_status_payload(Row) ->
    #{
        <<"phase">> => <<"idle">>,
        <<"state">> => maps:get(<<"state">>, Row),
        <<"generation">> => maps:get(<<"generation">>, Row),
        <<"failures">> => []
    }.

%% ===================================================================
%% Internal helpers
%% ===================================================================

clamp_limit(Limit) when is_integer(Limit), Limit >= 1, Limit =< 100 -> Limit;
clamp_limit(Limit) when is_integer(Limit), Limit > 100 -> 100;
clamp_limit(_) -> 50.

%% epgsql jsonb 列回读为 JSON binary；API 响应需展开为对象（坏值透传原样）。
decode_jsonb(Bin) when is_binary(Bin) ->
    try
        jsone:decode(Bin, [{object_format, map}])
    catch
        _:_ -> Bin
    end;
decode_jsonb(Other) ->
    Other.

decode_b64(Bin) when is_binary(Bin) ->
    try
        {ok, base64:decode(pad_base64(Bin))}
    catch
        _:_ -> {error, invalid_base64}
    end;
decode_b64(_) ->
    {error, invalid_base64}.

pad_base64(Bin) ->
    case byte_size(Bin) rem 4 of
        2 -> <<Bin/binary, "==">>;
        3 -> <<Bin/binary, "=">>;
        _ -> Bin
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
