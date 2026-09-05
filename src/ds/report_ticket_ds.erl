-module(report_ticket_ds).
%%%
% report_ticket_ds — G3 架构治理：report_logic 不应直调 report_ticket_repo
% G3: thin DS wrapper for report tickets
%%%

-include("log.hrl").

%% ==================== API ====================
-export([create/5]).
-export([create_message/9]).
-export([page_admin/3]).
-export([find_by_id/1]).
-export([resolve/4]).
-export([fetch_message_target/2]).

-spec create(binary(), integer(), integer(), binary(), binary()) ->
    {ok, integer()} | {error, any()}.
create(Type, TargetId, ReporterUid, Reason, Desc) ->
    report_ticket_repo:create(Type, TargetId, ReporterUid, Reason, Desc).

%% R-01: 消息类举报（target_type=message）携带 scope/author/结构化证据。
-spec create_message(
    integer(), binary(), integer(), integer(), integer(), binary(), binary(), map(), map()
) ->
    {ok, integer()} | {error, already_reported | any()}.
create_message(TargetId, SubType, ScopeId, AuthorId, ReporterUid, Reason, Desc, Evidence, _Extra) ->
    report_ticket_repo:create(
        <<"message">>, TargetId, SubType, ScopeId, AuthorId, ReporterUid, Reason, Desc, Evidence
    ).

-spec page_admin(map(), integer(), integer()) -> {ok, map()} | {error, any()}.
page_admin(RepoFilter, Page, Size) ->
    report_ticket_repo:page_admin(RepoFilter, Page, Size).

-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(ReportId) ->
    report_ticket_repo:find_by_id(ReportId).

-spec resolve(integer(), integer(), binary(), integer()) ->
    {ok, non_neg_integer()} | {error, any()}.
resolve(ReportId, Result, Note, AdmUid) ->
    report_ticket_repo:resolve(ReportId, Result, Note, AdmUid).

%% R-01: 举报前核验消息目标。统一形态：
%%   #{server_id, msg_id, msg_type, e2ee(bool), from_id, to_id, scope_id,
%%     author_id, revoked(bool), status(int|null), edited_at(binary|null),
%%     payload(binary|null), created_at}
%% 定位方式（客户端契约）：
%%   * c2c/c2g：Flutter Message.id = msg_id 字符串 → 按 msg_id 查行，
%%     落库 target_id 用服务端行 bigint ID（stable target）
%%   * channel：ChannelMessageModel.id = 服务端行 bigint → 按 id 查行
%% 不存在/已物理删除 → {error, not_found}（elib_pg:one/2 零行返回 {ok,#{}}）。
%% 服务端不解析 E2EE 密文（payload 原样返回，由 logic 决定是否读取；
%% E2EE 行 logic 只统计哈希/长度，不解密、不索取会话密钥）。
-spec fetch_message_target(c2c | c2g | channel, binary() | integer()) ->
    {ok, map()} | {error, not_found | any()}.
fetch_message_target(c2c, MsgKey) ->
    Sql =
        <<
            "SELECT id, from_id, to_id, msg_id, msg_type, e2ee, payload, created_at"
            " FROM public.msg_c2c WHERE msg_id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [MsgKey]) of
        {ok, [Row | _]} -> {ok, normalize_c2_row(Row)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end;
fetch_message_target(c2g, MsgKey) ->
    Sql =
        <<
            "SELECT id, from_id, to_id, msg_id, msg_type, e2ee, payload, created_at"
            " FROM public.msg_c2g WHERE msg_id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [MsgKey]) of
        {ok, [Row | _]} -> {ok, normalize_c2g_row(Row)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end;
fetch_message_target(channel, MsgKey) ->
    %% epgsql int codec 只吃 integer（binary 与数字比较恒判 overflow），
    %% channel 的行 ID 入参统一转 integer。
    MsgId = to_row_id(MsgKey),
    Sql =
        <<
            "SELECT m.id, m.channel_id, m.author_id, m.msg_type, m.content, m.status,"
            " m.revoked, m.edited_at, m.created_at,"
            " c.visibility AS channel_visibility, c.status AS channel_status"
            " FROM public.channel_message m"
            " LEFT JOIN public.channel c ON c.id = m.channel_id"
            " WHERE m.id = $1 LIMIT 1"
        >>,
    case elib_pg:one(Sql, [MsgId]) of
        {ok, Row} when is_map(Row), map_size(Row) > 0 -> {ok, normalize_channel_row(Row)};
        {ok, _} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%%% ==================== internal ====================

-spec normalize_c2_row(map()) -> map().
normalize_c2_row(Row) ->
    #{
        server_id => to_int(maps:get(<<"id">>, Row, 0)),
        msg_id => maps:get(<<"msg_id">>, Row, <<>>),
        msg_type => maps:get(<<"msg_type">>, Row, <<>>),
        e2ee => is_e2ee_envelope(maps:get(<<"e2ee">>, Row, null)),
        from_id => to_int(maps:get(<<"from_id">>, Row, 0)),
        to_id => to_int(maps:get(<<"to_id">>, Row, 0)),
        scope_id => to_int(maps:get(<<"to_id">>, Row, 0)),
        author_id => to_int(maps:get(<<"from_id">>, Row, 0)),
        revoked => false,
        status => 1,
        edited_at => maps:get(<<"edited_at">>, Row, null),
        payload => maps:get(<<"payload">>, Row, <<>>),
        created_at => maps:get(<<"created_at">>, Row, null)
    }.

-spec normalize_c2g_row(map()) -> map().
normalize_c2g_row(Row) ->
    #{
        server_id => to_int(maps:get(<<"id">>, Row, 0)),
        msg_id => maps:get(<<"msg_id">>, Row, <<>>),
        msg_type => maps:get(<<"msg_type">>, Row, <<>>),
        e2ee => is_e2ee_envelope(maps:get(<<"e2ee">>, Row, null)),
        from_id => to_int(maps:get(<<"from_id">>, Row, 0)),
        to_id => to_int(maps:get(<<"to_id">>, Row, 0)),
        scope_id => to_int(maps:get(<<"to_id">>, Row, 0)),
        author_id => to_int(maps:get(<<"from_id">>, Row, 0)),
        revoked => false,
        status => 1,
        edited_at => maps:get(<<"edited_at">>, Row, null),
        payload => maps:get(<<"payload">>, Row, <<>>),
        created_at => maps:get(<<"created_at">>, Row, null)
    }.

-spec normalize_channel_row(map()) -> map().
normalize_channel_row(Row) ->
    #{
        server_id => to_int(maps:get(<<"id">>, Row, 0)),
        msg_id => integer_to_binary(to_int(maps:get(<<"id">>, Row, 0))),
        msg_type => maps:get(<<"msg_type">>, Row, <<>>),
        e2ee => false,
        from_id => to_int(maps:get(<<"author_id">>, Row, 0)),
        to_id => to_int(maps:get(<<"channel_id">>, Row, 0)),
        scope_id => to_int(maps:get(<<"channel_id">>, Row, 0)),
        author_id => to_int(maps:get(<<"author_id">>, Row, 0)),
        revoked => maps:get(<<"revoked">>, Row, false) =:= true,
        status => to_int(maps:get(<<"status">>, Row, 1)),
        edited_at => maps:get(<<"edited_at">>, Row, null),
        payload => maps:get(<<"content">>, Row, <<>>),
        created_at => maps:get(<<"created_at">>, Row, null),
        %% 频道可见性（JOIN channel）：status=1 且 visibility=0 为公开频道，
        %% 未订阅的浏览者也有权举报其中消息
        channel_public =>
            to_int(maps:get(<<"channel_status">>, Row, 0)) =:= 1 andalso
            to_int(maps:get(<<"channel_visibility">>, Row, -1)) =:= 0
    }.

-spec to_int(term()) -> integer().
to_int(Value) when is_integer(Value) ->
    Value;
to_int(Value) when is_binary(Value) ->
    try
        binary_to_integer(Value)
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.

-spec to_row_id(integer() | binary()) -> integer().
to_row_id(Int) when is_integer(Int) ->
    Int;
to_row_id(Bin) when is_binary(Bin) ->
    try
        binary_to_integer(Bin)
    catch
        _:_ -> 0
    end;
to_row_id(_) ->
    0.

%% e2ee jsonb 列：SQL NULL（undefined）= 未加密；map（jsonb codec）或
%% binary JSON 文本（epgsql 默认）= 信封。空对象/null 字面量不算加密。
-spec is_e2ee_envelope(term()) -> boolean().
is_e2ee_envelope(undefined) ->
    false;
is_e2ee_envelope(null) ->
    false;
is_e2ee_envelope(Value) when is_map(Value) ->
    map_size(Value) > 0;
is_e2ee_envelope(Value) when is_binary(Value) ->
    Trimmed = string:trim(Value),
    Trimmed =/= <<>> andalso Trimmed =/= <<"null">> andalso Trimmed =/= <<"{}">>;
is_e2ee_envelope(_) ->
    false.
