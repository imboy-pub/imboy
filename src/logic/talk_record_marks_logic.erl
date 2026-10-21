-module(talk_record_marks_logic).
-moduledoc "面聊重点标记业务逻辑（T07）：位置解析（capture 真源/投影）、幂等、If-Match。".
%%%
% marks 业务逻辑（§4 marks API + §11.1 裁决 + 00000213）。
%
% 位置语义（裁决）：
%   * 写路径真源 = capture_id + capture_offset_ms；position_ms 为 V1 单
%     capture 投影（写时 position_ms := capture_offset_ms，同时维护两者）；
%   * 请求给 position_ms（无 capture_id）→ 服务端解析目标 capture：
%     record.active_capture_id 优先，否则最近一条 capture；无 capture → 422；
%   * capture_offset / position 必须落在有效录音范围内（capture.duration_ms，
%     即有效音频累加，排除暂停墙钟）→ 越界 422 mark_position_out_of_range；
%   * 幂等：client_mark_id（UNIQUE(record_id,author_id,client_mark_id)）；
%     同 id 异内容 → 409 mark_conflict。
%%%

-export([create/3]).
-export([list/3]).
-export([update/5]).
-export([delete/4]).
-export([resolve_position_tx/3]).
-export([parse_mark_position/1]).
-export([valid_mark_text/1]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(MAX_MARK_CHARS, 1000).
-define(DEFAULT_MARKS_LIMIT, 100).

%% ===================================================================
%% Facade（Handler 入口）
%% ===================================================================

%% @doc POST /:id/marks：创建/幂等重放。
%% 返回 {ok, created | replay, Payload} | {error, Reason}。
-spec create(integer(), integer(), map()) -> {ok, created | replay, map()} | {error, term()}.
create(Uid, RecordId, Body) when is_map(Body) ->
    case parse_mark_position(Body) of
        {ok, PosSpec} ->
            case valid_mark_text(maps:get(<<"text">>, Body, <<>>)) of
                true ->
                    create_with_position(Uid, RecordId, PosSpec, Body);
                false ->
                    {error, invalid_mark_text}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
create(_Uid, _RecordId, _Body) ->
    {error, invalid_body}.

-spec create_with_position(integer(), integer(), map(), map()) ->
    {ok, created | replay, map()} | {error, term()}.
create_with_position(Uid, RecordId, PosSpec, Body) ->
    ClientMarkId = maps:get(client_mark_id, PosSpec),
    Text = maps:get(<<"text">>, Body, <<>>),
    %% ACL 与 T05 update 同口径：repo 统一可见性谓词（owner/grant/企业归属）
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, _Row} ->
            run_mark_create(Uid, RecordId, PosSpec, ClientMarkId, Text);
        {error, Reason} ->
            {error, Reason}
    end.

-spec run_mark_create(integer(), integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
run_mark_create(Uid, RecordId, PosSpec, ClientMarkId, Text) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> create_tx(Conn, Uid, RecordId, PosSpec, Text) end,
            [{reraise, false}]
        )
    of
        {ok, Mark, created} ->
            {ok, created, mark_payload(Mark)};
        {ok, Mark, replay} ->
            {ok, replay, mark_payload(Mark)};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_mark_create_rolled_back, Uid, ClientMarkId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 事务体：位置解析（事务内一致）→ 幂等比对 → 插入。
-spec create_tx(any(), integer(), integer(), map(), binary()) ->
    {ok, map(), created | replay} | {error, term()}.
create_tx(Conn, Uid, RecordId, PosSpec, Text) ->
    resolve_or_rollback(Conn, RecordId, PosSpec, Uid, Text).

-spec resolve_or_rollback(any(), integer(), map(), integer(), binary()) ->
    {ok, map(), created | replay} | {error, term()}.
resolve_or_rollback(Conn, RecordId, PosSpec, Uid, Text) ->
    case resolve_position_tx(Conn, RecordId, PosSpec) of
        {ok, #{capture_id := CaptureId, capture_offset_ms := Offset}} ->
            mark_idempotent_tx(Conn, Uid, RecordId, PosSpec, CaptureId, Offset, Text);
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private 幂等比对：同 client_mark_id 同内容 → 重放；异内容 → 409。
-spec mark_idempotent_tx(any(), integer(), integer(), map(), integer(), integer(), binary()) ->
    {ok, map(), created | replay} | {error, term()}.
mark_idempotent_tx(Conn, Uid, RecordId, PosSpec, CaptureId, Offset, Text) ->
    ClientMarkId = maps:get(client_mark_id, PosSpec),
    case talk_record_marks_repo:find_by_client_id_tx(Conn, RecordId, Uid, ClientMarkId) of
        {error, not_found} ->
            insert_mark_tx(Conn, Uid, RecordId, PosSpec, CaptureId, Offset, Text);
        {ok, Existing} ->
            case mark_same(Existing, CaptureId, Offset, Text) of
                true -> {ok, Existing, replay};
                false -> {error, mark_conflict}
            end;
        {error, Reason} ->
            ?ERROR_LOG([talk_record_mark_lookup_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec mark_same(map(), integer() | null, integer(), binary()) -> boolean().
mark_same(
    #{<<"capture_id">> := C, <<"capture_offset_ms">> := O, <<"text">> := T},
    CaptureId,
    Offset,
    Text
) ->
    norm_capture(C) =:= norm_capture(CaptureId) andalso O =:= Offset andalso T =:= Text.

-spec norm_capture(integer() | null | binary()) -> integer() | null.
norm_capture(null) -> null;
norm_capture(N) when is_integer(N) -> N.

-spec insert_mark_tx(any(), integer(), integer(), map(), integer() | null, integer(), binary()) ->
    {ok, map(), created} | {error, term()}.
insert_mark_tx(Conn, Uid, RecordId, PosSpec, CaptureId, Offset, Text) ->
    Now = elib_dt:now(),
    Data = #{
        <<"id">> => elib_tsid:generate(talk_record),
        <<"record_id">> => RecordId,
        <<"author_id">> => Uid,
        <<"client_mark_id">> => maps:get(client_mark_id, PosSpec),
        <<"capture_id">> => CaptureId,
        <<"capture_offset_ms">> => Offset,
        %% V1 单 capture 投影：position_ms := capture_offset_ms（裁决）
        <<"position_ms">> => Offset,
        <<"text">> => Text,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },
    case talk_record_marks_repo:insert_tx(Conn, Data) of
        {ok, MarkId} ->
            case talk_record_marks_repo:find_tx(Conn, RecordId, MarkId) of
                {ok, Row} -> {ok, Row, created};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            ?ERROR_LOG([talk_record_mark_insert_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @doc GET /:id/marks：position_ms/id 正序（§4）；跟随记录 ACL。
-spec list(integer(), integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list(Uid, RecordId, Limit) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, _Row} ->
            case
                elib_pg:with_tx(fun(Conn) ->
                    talk_record_marks_repo:page_by_record_tx(Conn, RecordId, Limit)
                end)
            of
                {ok, Rows} ->
                    {ok, #{<<"marks">> => [mark_payload(R) || R <- Rows]}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc PATCH /:id/marks/:mark_id（作者本人；text / capture_offset_ms；If-Match）。
-spec update(integer(), integer(), integer(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
update(Uid, RecordId, MarkId, IfMatch, Body) when is_map(Body) ->
    case talk_record_logic:parse_if_match(IfMatch) of
        {ok, ExpectedRevision} ->
            update_parsed(Uid, RecordId, MarkId, ExpectedRevision, Body);
        {error, Reason} ->
            {error, Reason}
    end;
update(_Uid, _RecordId, _MarkId, _IfMatch, _Body) ->
    {error, invalid_body}.

-spec update_parsed(integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_parsed(Uid, RecordId, MarkId, ExpectedRevision, Body) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, _Row} ->
            update_visible(Uid, RecordId, MarkId, ExpectedRevision, Body);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_visible(integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_visible(Uid, RecordId, MarkId, ExpectedRevision, Body) ->
    Picked = maps:with([<<"text">>, <<"capture_offset_ms">>], Body),
    case map_size(Picked) of
        0 ->
            {error, missing_editable_field};
        _ ->
            update_fields(Uid, RecordId, MarkId, ExpectedRevision, Picked)
    end.

-spec update_fields(integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_fields(Uid, RecordId, MarkId, ExpectedRevision, Picked) ->
    case valid_update_fields(Picked) of
        ok ->
            case
                elib_pg:with_tx(
                    fun(Conn) ->
                        update_tx(Conn, Uid, RecordId, MarkId, ExpectedRevision, Picked)
                    end,
                    [{reraise, false}]
                )
            of
                {ok, Row} ->
                    {ok, mark_payload(Row)};
                {rollback, {talk_record_rollback, {error, Reason}}} ->
                    {error, Reason};
                {rollback, Reason} ->
                    ?ERROR_LOG([talk_record_mark_update_rolled_back, MarkId, Reason]),
                    {error, db_error};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec valid_update_fields(map()) -> ok | {error, atom()}.
valid_update_fields(Picked) ->
    Checks = [
        {<<"text">>, fun valid_mark_text/1, invalid_mark_text},
        {<<"capture_offset_ms">>, fun valid_offset/1, invalid_capture_offset}
    ],
    validate_fields(Checks, Picked, ok).

-spec validate_fields([{binary(), function(), atom()}], map(), ok | {error, atom()}) ->
    ok | {error, atom()}.
validate_fields(_, _, {error, Reason}) ->
    {error, Reason};
validate_fields([], _Picked, ok) ->
    ok;
validate_fields([{Key, Check, Reason} | Rest], Picked, ok) ->
    case maps:find(Key, Picked) of
        {ok, Value} ->
            case Check(Value) of
                true -> validate_fields(Rest, Picked, ok);
                false -> validate_fields(Rest, Picked, {error, Reason})
            end;
        error ->
            validate_fields(Rest, Picked, ok)
    end.

%% @private 事务体：作者校验 → 位移重校验 → CAS。
-spec update_tx(any(), integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_tx(Conn, Uid, RecordId, MarkId, ExpectedRevision, Picked) ->
    case talk_record_marks_repo:find_tx(Conn, RecordId, MarkId) of
        {ok, #{<<"author_id">> := Uid} = Mark} ->
            update_author_tx(Conn, RecordId, Mark, MarkId, ExpectedRevision, Picked);
        {ok, _Other} ->
            {error, mark_not_author};
        {error, not_found} ->
            {error, mark_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_author_tx(any(), integer(), map(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_author_tx(Conn, RecordId, Mark, MarkId, ExpectedRevision, Picked) ->
    case maps:find(<<"capture_offset_ms">>, Picked) of
        {ok, NewOffset} ->
            reposition_tx(Conn, RecordId, Mark, MarkId, ExpectedRevision, Picked, NewOffset);
        error ->
            cas_update_tx(Conn, MarkId, ExpectedRevision, Picked)
    end.

%% @private 位移：沿用 mark 既有 capture（capture_id 不可经 PATCH 更换——
%% 换绑属新 mark 语义）；范围对 capture.duration_ms 重校验。
-spec reposition_tx(any(), integer(), map(), integer(), pos_integer(), map(), integer()) ->
    {ok, map()} | {error, term()}.
reposition_tx(Conn, RecordId, Mark, MarkId, ExpectedRevision, Picked, NewOffset) ->
    case maps:get(<<"capture_id">>, Mark, null) of
        null ->
            {error, mark_no_capture};
        CaptureId when is_integer(CaptureId) ->
            case talk_record_capture_repo:find_tx(Conn, RecordId, CaptureId, <<"c.duration_ms">>) of
                {ok, #{<<"duration_ms">> := Duration}} ->
                    case NewOffset =< Duration of
                        true ->
                            cas_update_tx(
                                Conn,
                                MarkId,
                                ExpectedRevision,
                                Picked#{<<"position_ms">> => NewOffset}
                            );
                        false ->
                            {error, mark_position_out_of_range}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _Other ->
            {error, mark_no_capture}
    end.

-spec cas_update_tx(any(), integer(), pos_integer(), map()) -> {ok, map()} | {error, term()}.
cas_update_tx(Conn, MarkId, ExpectedRevision, Sets) ->
    case talk_record_marks_repo:update_tx(Conn, MarkId, ExpectedRevision, Sets) of
        {ok, Row} ->
            {ok, Row};
        {error, not_found} ->
            {error, precondition_failed};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc DELETE /:id/marks/:mark_id（作者或记录 owner；If-Match）。
-spec delete(integer(), integer(), integer(), binary() | undefined) ->
    {ok, map()} | {error, term()}.
delete(Uid, RecordId, MarkId, IfMatch) ->
    case talk_record_logic:parse_if_match(IfMatch) of
        {ok, ExpectedRevision} ->
            delete_parsed(Uid, RecordId, MarkId, ExpectedRevision);
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_parsed(integer(), integer(), integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
delete_parsed(Uid, RecordId, MarkId, ExpectedRevision) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, Row} ->
            delete_visible(Uid, RecordId, MarkId, ExpectedRevision, Row);
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_visible(integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
delete_visible(Uid, RecordId, MarkId, ExpectedRevision, RecordRow) ->
    IsOwner = maps:get(<<"owner_user_id">>, RecordRow) =:= Uid,
    case
        elib_pg:with_tx(
            fun(Conn) -> delete_tx(Conn, Uid, RecordId, MarkId, ExpectedRevision, IsOwner) end,
            [{reraise, false}]
        )
    of
        {ok, deleted} ->
            {ok, #{<<"mark_id">> => MarkId, <<"deleted">> => true}};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_mark_delete_rolled_back, MarkId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_tx(any(), integer(), integer(), integer(), pos_integer(), boolean()) ->
    {ok, deleted} | {error, term()}.
delete_tx(Conn, Uid, RecordId, MarkId, ExpectedRevision, IsOwner) ->
    case talk_record_marks_repo:find_tx(Conn, RecordId, MarkId) of
        {ok, #{<<"author_id">> := AuthorId}} when AuthorId =:= Uid; IsOwner ->
            case talk_record_marks_repo:delete_tx(Conn, MarkId, ExpectedRevision) of
                {ok, deleted} -> {ok, deleted};
                {error, not_found} -> {error, precondition_failed};
                {error, Reason} -> {error, Reason}
            end;
        {ok, _} ->
            {error, mark_not_author};
        {error, not_found} ->
            {error, mark_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% 位置解析（事务内；create 与测试共用）
%% ===================================================================

%% @doc 位置解析（§11.1 裁决）：
%%   * 显式 capture_id + capture_offset_ms → 校验 capture 同记录 + 范围；
%%   * 仅 position_ms → 解析 active/最近 capture 后校验范围；
%%   * 无 capture 可绑 → {error, no_capture_for_position}。
%% 返回 #{capture_id => integer() | null, capture_offset_ms => integer()}。
%% 注：V1 记录最多一条 capture（uq_trc_record_active），capture_id NULL 仅
%% 出现在无采集历史记录（当前不可能经此路径产生）。
-spec resolve_position_tx(any(), integer(), map()) ->
    {ok, #{capture_id := integer() | null, capture_offset_ms := integer()}} | {error, atom()}.
resolve_position_tx(Conn, RecordId, #{explicit := #{capture_id := CaptureId, offset := Offset}}) ->
    Columns = <<"c.id, c.duration_ms">>,
    case talk_record_capture_repo:find_tx(Conn, RecordId, CaptureId, Columns) of
        {ok, #{<<"duration_ms">> := Duration}} ->
            position_in_range(Conn, RecordId, CaptureId, Offset, Duration);
        {error, not_found} ->
            {error, capture_not_found};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_mark_capture_lookup_failed, RecordId, Reason]),
            {error, db_error}
    end;
resolve_position_tx(Conn, RecordId, #{implicit := Position}) ->
    case target_capture_tx(Conn, RecordId) of
        {ok, #{<<"id">> := CaptureId, <<"duration_ms">> := Duration}} ->
            position_in_range(Conn, RecordId, CaptureId, Position, Duration);
        {error, Reason} ->
            {error, Reason}
    end.

-spec position_in_range(any(), integer(), integer(), integer(), integer()) ->
    {ok, #{capture_id := integer(), capture_offset_ms := integer()}} | {error, atom()}.
position_in_range(_Conn, _RecordId, CaptureId, Offset, Duration) when
    Offset >= 0, Offset =< Duration
->
    {ok, #{capture_id => CaptureId, capture_offset_ms => Offset}};
position_in_range(_Conn, _RecordId, _CaptureId, _Offset, _Duration) ->
    {error, mark_position_out_of_range}.

%% @private 目标 capture：active_capture_id 优先，否则最近一条（§4 恢复语义）。
-spec target_capture_tx(any(), integer()) -> {ok, map()} | {error, atom()}.
target_capture_tx(Conn, RecordId) ->
    Sql =
        <<"SELECT c.id, c.duration_ms FROM talk_record_capture c", " WHERE c.record_id = $1",
            " ORDER BY (CASE WHEN c.id = (SELECT tr.active_capture_id FROM talk_record tr",
            "   WHERE tr.id = $1) THEN 0 ELSE 1 END), c.created_at DESC, c.id DESC", " LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            {error, no_capture_for_position};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_mark_target_capture_failed, RecordId, Reason]),
            {error, db_error}
    end.

%% @doc 请求体位置归一化：
%%   {ok, #{explicit := #{capture_id, offset}, client_mark_id}}（显式真源）|
%%   {ok, #{implicit := Position, client_mark_id}}（投影解析）| {error, Reason}。
-spec parse_mark_position(map()) -> {ok, map()} | {error, atom()}.
parse_mark_position(Body) ->
    ClientMarkId = maps:get(<<"client_mark_id">>, Body, <<>>),
    case is_uuid(ClientMarkId) of
        false ->
            {error, invalid_client_mark_id};
        true ->
            parse_position_fields(Body, #{client_mark_id => ClientMarkId})
    end.

-spec parse_position_fields(map(), map()) -> {ok, map()} | {error, atom()}.
parse_position_fields(Body, Acc) ->
    HasCapture = maps:is_key(<<"capture_id">>, Body),
    HasOffset = maps:is_key(<<"capture_offset_ms">>, Body),
    HasPosition = maps:is_key(<<"position_ms">>, Body),
    parse_position_branch(Body, Acc, {HasCapture, HasOffset, HasPosition}).

-spec parse_position_branch(map(), map(), {boolean(), boolean(), boolean()}) ->
    {ok, map()} | {error, atom()}.
%% 显式真源：capture_id + capture_offset_ms（position_ms 同时给则必须一致）
parse_position_branch(Body, Acc, {true, true, _}) ->
    case
        {
            nonneg_int(maps:get(<<"capture_id">>, Body)),
            nonneg_int(maps:get(<<"capture_offset_ms">>, Body))
        }
    of
        {{ok, CaptureId}, {ok, Offset}} ->
            consistent_position(Body, Offset, Acc#{
                explicit => #{capture_id => CaptureId, offset => Offset}
            });
        _ ->
            {error, invalid_capture_offset}
    end;
parse_position_branch(_Body, _Acc, {true, false, _}) ->
    {error, missing_capture_offset};
%% 仅投影 position_ms → 服务端解析 capture
parse_position_branch(Body, Acc, {false, _, true}) ->
    case nonneg_int(maps:get(<<"position_ms">>, Body)) of
        {ok, Position} -> {ok, Acc#{implicit => Position}};
        _ -> {error, invalid_position_ms}
    end;
parse_position_branch(_Body, _Acc, {false, _, false}) ->
    {error, missing_position}.

-spec consistent_position(map(), integer(), map()) -> {ok, map()} | {error, atom()}.
consistent_position(Body, Offset, Acc) ->
    case maps:find(<<"position_ms">>, Body) of
        {ok, Position} when Position =:= Offset -> {ok, Acc};
        {ok, _Other} -> {error, position_projection_mismatch};
        error -> {ok, Acc}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec mark_payload(map()) -> map().
mark_payload(Mark) ->
    Offset = maps:get(<<"capture_offset_ms">>, Mark, 0),
    #{
        <<"id">> => maps:get(<<"id">>, Mark),
        <<"author_id">> => maps:get(<<"author_id">>, Mark),
        <<"client_mark_id">> => maps:get(<<"client_mark_id">>, Mark),
        <<"capture_id">> => maps:get(<<"capture_id">>, Mark),
        %% 读路径：以 offset 真源投影 position（裁决）
        <<"capture_offset_ms">> => Offset,
        <<"position_ms">> => Offset,
        <<"text">> => maps:get(<<"text">>, Mark),
        <<"revision">> => maps:get(<<"revision">>, Mark, 1),
        <<"etag">> => talk_record_ds:etag(maps:get(<<"revision">>, Mark, 1)),
        <<"created_at">> => maps:get(<<"created_at">>, Mark),
        <<"updated_at">> => maps:get(<<"updated_at">>, Mark)
    }.

%% @doc mark 文本校验（varchar(1000)，§2）。
-spec valid_mark_text(term()) -> boolean().
valid_mark_text(Text) when is_binary(Text) ->
    string:length(Text) =< ?MAX_MARK_CHARS;
valid_mark_text(_) ->
    false.

-spec valid_offset(term()) -> boolean().
valid_offset(N) when is_integer(N), N >= 0 ->
    true;
valid_offset(_) ->
    false.

-spec nonneg_int(term()) -> {ok, integer()} | error.
nonneg_int(N) when is_integer(N), N >= 0 -> {ok, N};
nonneg_int(N) when is_binary(N) ->
    try
        nonneg_int(binary_to_integer(N))
    catch
        _:_ -> error
    end;
nonneg_int(_) ->
    error.

-spec is_uuid(binary()) -> boolean().
is_uuid(Bin) ->
    Re = <<"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$">>,
    try
        case re:run(Bin, Re) of
            {match, _} -> true;
            nomatch -> false
        end
    catch
        _:_ -> false
    end.
