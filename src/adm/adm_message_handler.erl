-module(adm_message_handler).
%%%
% adm_message 控制器模块
% 消息管理 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(EXPORT_CHUNK_SIZE, 1000).

-ifdef(EUNIT).
-export([normalize_scope/1, parse_conversation/1, normalize_ts/1, build_union_sql/1]).
-export([csv_escape/1, row_to_csv_line/1]).
-export([sanitize_row_by_audit_mode/2]).
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            list -> list(Method, Req0, State);
            detail -> detail(Method, Req0, State);
            export -> export(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 消息列表
-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, _State) ->
    case ensure_message_audit_enabled(Req0) of
        ok ->
            AuditMode = imboy_policy:message_audit_mode(),
            {Page, Size} = elib_param:page(Req0),
            Filters = extract_filters(Req0),
            Params = build_params(Filters),

            UnionSql = build_union_sql(Filters),
            CountSql = <<"SELECT COUNT(*) AS count FROM (", UnionSql/binary, ") m">>,
            case elib_pg:one(CountSql, Params) of
                {ok, CountMap} ->
                    Total = get_count(CountMap),
                    case Total > 0 of
                        true ->
                            Offset = (Page - 1) * Size,
                            LimitPos = integer_to_binary(length(Params) + 1),
                            OffsetPos = integer_to_binary(length(Params) + 2),
                            DataSql = iolist_to_binary([
                                <<"SELECT scope, msg_id, from_id, to_id, msg_type, action, payload, created_at, server_ts ">>,
                                <<"FROM (">>, UnionSql, <<") m ">>,
                                <<"ORDER BY created_at DESC, msg_id DESC ">>,
                                <<"LIMIT $">>, LimitPos, <<" OFFSET $">>, OffsetPos
                            ]),
                            case elib_pg:query(DataSql, Params ++ [Size, Offset]) of
                                {ok, Rows} ->
                                    Items = sanitize_rows_by_audit_mode(
                                        [normalize_row(Row) || Row <- Rows],
                                        AuditMode
                                    ),
                                    elib_response:success(Req0, #{
                                        list => Items,
                                        total => Total,
                                        page => Page,
                                        size => Size
                                    });
                                {error, _Reason} ->
                                    elib_response:error(Req0, "查询失败")
                            end;
                        false ->
                            elib_response:success(Req0, #{
                                list => [],
                                total => 0,
                                page => Page,
                                size => Size
                            })
                    end;
                {error, _Reason} ->
                    elib_response:error(Req0, "查询失败")
            end;
        {error, Req1} ->
            Req1
    end;
list(_, Req0, _State) ->
    Req0.

%% @doc 消息详情
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, _State) ->
    case ensure_message_audit_enabled(Req0) of
        ok ->
            AuditMode = imboy_policy:message_audit_mode(),
            {ok, MsgId} = elib_param:binary(msg_id, Req0, <<>>),
            case MsgId of
                <<>> ->
                    elib_response:error(Req0, "参数错误");
                _ ->
                    Filters0 = extract_filters(Req0),
                    Filters = Filters0#{msg_id => MsgId},
                    Params = build_params(Filters),
                    UnionSql = build_union_sql(Filters),
                    DataSql = <<"SELECT scope, msg_id, from_id, to_id, msg_type, action, payload, created_at, server_ts "
                                "FROM (", UnionSql/binary, ") m ORDER BY created_at DESC LIMIT 1">>,
                    case elib_pg:query(DataSql, Params) of
                        {ok, [Row | _]} ->
                            elib_response:success(
                                Req0,
                                sanitize_row_by_audit_mode(normalize_row(Row), AuditMode)
                            );
                        {ok, []} ->
                            elib_response:error(Req0, "消息不存在");
                        {error, _Reason} ->
                            elib_response:error(Req0, "查询失败")
                    end
            end;
        {error, Req1} ->
            Req1
    end;
detail(_, Req0, _State) ->
    Req0.

-spec export(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
export(<<"GET">>, Req0, _State) ->
    case ensure_message_export_enabled(Req0) of
        ok ->
            Filters = extract_filters(Req0),
            Params = build_params(Filters),
            UnionSql = build_union_sql(Filters),
            Headers = #{
                <<"content-type">> => <<"text/csv; charset=utf-8">>,
                <<"content-disposition">> => <<"attachment; filename=\"messages_export.csv\"">>,
                <<"cache-control">> => <<"no-store">>
            },
            Req1 = cowboy_req:stream_reply(200, Headers, Req0),
            ok = cowboy_req:stream_body(csv_header_with_bom(), nofin, Req1),
            stream_export_rows(
                Req1,
                UnionSql,
                Params,
                ?EXPORT_CHUNK_SIZE,
                0,
                imboy_policy:message_audit_mode()
            );
        {error, Req1} ->
            Req1
    end;
export(_, Req0, _State) ->
    Req0.

-spec stream_export_rows(cowboy_req:req(), binary(), list(), pos_integer(), non_neg_integer(), none | metadata | full) -> ok.
stream_export_rows(Req, UnionSql, Params, Limit, Offset, AuditMode) ->
    LimitPos = integer_to_binary(length(Params) + 1),
    OffsetPos = integer_to_binary(length(Params) + 2),
    DataSql = iolist_to_binary([
        <<"SELECT scope, msg_id, from_id, to_id, msg_type, action, payload, created_at, server_ts ">>,
        <<"FROM (">>, UnionSql, <<") m ">>,
        <<"ORDER BY created_at DESC, msg_id DESC ">>,
        <<"LIMIT $">>, LimitPos, <<" OFFSET $">>, OffsetPos
    ]),
    case elib_pg:query(DataSql, Params ++ [Limit, Offset]) of
        {ok, []} ->
            cowboy_req:stream_body(<<>>, fin, Req);
        {ok, Rows} ->
            NormalizedRows = sanitize_rows_by_audit_mode(
                [normalize_row(Row) || Row <- Rows],
                AuditMode
            ),
            CsvChunk = rows_to_csv_chunk(NormalizedRows),
            ok = cowboy_req:stream_body(CsvChunk, nofin, Req),
            case length(Rows) < Limit of
                true ->
                    cowboy_req:stream_body(<<>>, fin, Req);
                false ->
                    stream_export_rows(Req, UnionSql, Params, Limit, Offset + Limit, AuditMode)
            end;
        {error, _Reason} ->
            cowboy_req:stream_body(<<>>, fin, Req)
    end.

-spec csv_header_with_bom() -> binary().
csv_header_with_bom() ->
    <<239, 187, 191, "scope,msg_id,from_id,to_id,msg_type,action,payload,created_at,server_ts\n">>.

-spec rows_to_csv_chunk([map()]) -> binary().
rows_to_csv_chunk(Rows) ->
    iolist_to_binary([[row_to_csv_line(Row), <<"\n">>] || Row <- Rows]).

-spec row_to_csv_line(map()) -> iolist().
row_to_csv_line(Row) ->
    Fields = [
        maps:get(scope, Row, <<>>),
        maps:get(msg_id, Row, <<>>),
        maps:get(from_id, Row, 0),
        maps:get(to_id, Row, 0),
        maps:get(msg_type, Row, <<>>),
        maps:get(action, Row, <<>>),
        maps:get(payload, Row, <<>>),
        maps:get(created_at, Row, <<>>),
        maps:get(server_ts, Row, <<>>)
    ],
    lists:join(<<",">>, [csv_escape(Field) || Field <- Fields]).

-spec csv_escape(term()) -> binary().
csv_escape(Value) ->
    Bin = case Value of
        V when is_binary(V) -> V;
        V when is_integer(V) -> integer_to_binary(V);
        V when is_float(V) -> ec_cnv:to_binary(V);
        null -> <<>>;
        undefined -> <<>>;
        _ -> ec_cnv:to_binary(Value)
    end,
    Escaped = binary:replace(Bin, <<"\"">>, <<"\"\"">>, [global]),
    case needs_quote(Escaped) of
        true -> <<"\"", Escaped/binary, "\"">>;
        false -> Escaped
    end.

-spec needs_quote(binary()) -> boolean().
needs_quote(Bin) ->
    binary:match(Bin, <<",">>) =/= nomatch orelse
    binary:match(Bin, <<"\"">>) =/= nomatch orelse
    binary:match(Bin, <<"\n">>) =/= nomatch orelse
    binary:match(Bin, <<"\r">>) =/= nomatch.

-spec extract_filters(cowboy_req:req()) -> map().
extract_filters(Req0) ->
    {ok, Scope0} = elib_param:binary(msg_scope, Req0, <<"all">>),
    Uid = parse_uid_param(Req0),
    {ok, Conversation} = elib_param:binary(conversation, Req0, <<>>),
    {ok, FromTs0} = elib_param:binary(from_ts, Req0, <<>>),
    {ok, ToTs0} = elib_param:binary(to_ts, Req0, <<>>),
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {ConvA, ConvB, ConvGid} = parse_conversation(Conversation),
    #{
        scope => normalize_scope(Scope0),
        uid => uid_or_zero(Uid),
        conv_a => ConvA,
        conv_b => ConvB,
        conv_gid => ConvGid,
        from_ts => normalize_ts(FromTs0),
        to_ts => normalize_ts(ToTs0),
        keyword_like => keyword_like(Keyword),
        msg_id => <<>>
    }.

-spec build_params(map()) -> list().
build_params(Filters) ->
    [
        maps:get(uid, Filters, 0),
        maps:get(conv_a, Filters, 0),
        maps:get(conv_b, Filters, 0),
        maps:get(conv_gid, Filters, 0),
        maps:get(from_ts, Filters, <<>>),
        maps:get(to_ts, Filters, <<>>),
        maps:get(keyword_like, Filters, <<>>),
        maps:get(msg_id, Filters, <<>>)
    ].

-spec build_union_sql(map()) -> binary().
build_union_sql(Filters) ->
    Scopes = scopes_for(maps:get(scope, Filters, <<"all">>)),
    message_ds:build_adm_union_sql(Scopes).


-spec normalize_scope(binary()) -> binary().
normalize_scope(Scope0) ->
    Scope = to_lower_binary(Scope0),
    case Scope of
        <<"c2c">> -> <<"c2c">>;
        <<"c2g">> -> <<"c2g">>;
        <<"c2s">> -> <<"c2s">>;
        <<"s2c">> -> <<"s2c">>;
        _ -> <<"all">>
    end.

-spec scopes_for(binary()) -> [c2c | c2g | c2s | s2c].
scopes_for(<<"c2c">>) -> [c2c];
scopes_for(<<"c2g">>) -> [c2g];
scopes_for(<<"c2s">>) -> [c2s];
scopes_for(<<"s2c">>) -> [s2c];
scopes_for(_) -> [c2c, c2g, c2s, s2c].

-spec parse_conversation(binary()) -> {integer(), integer(), integer()}.
parse_conversation(<<>>) ->
    {0, 0, 0};
parse_conversation(Conversation) ->
    Tokens0 = re:split(Conversation, <<"[,:_]">>, [{return, binary}]),
    Tokens = [T || T <- Tokens0, T =/= <<>>],
    case Tokens of
        [A0, B0] ->
            A = to_positive_int(A0),
            B = to_positive_int(B0),
            case A > 0 andalso B > 0 of
                true when A =/= B -> {A, B, 0};
                _ -> {0, 0, 0}
            end;
        [Gid0] ->
            {0, 0, to_positive_int(Gid0)};
        _ ->
            {0, 0, 0}
    end.

-spec parse_uid_param(cowboy_req:req()) -> integer().
parse_uid_param(Req0) ->
    Uid0 =
        case catch elib_param:int(uid, Req0, 0) of
            {ok, Uid} when is_integer(Uid), Uid > 0 ->
                Uid;
            _ ->
                0
        end,
    case Uid0 > 0 of
        true ->
            Uid0;
        false ->
            case catch elib_param:binary(uid, Req0, <<>>) of
                {ok, UidBin} ->
                    parse_id(UidBin);
                _ ->
                    0
            end
    end.

-spec normalize_ts(binary()) -> binary().
normalize_ts(<<>>) ->
    <<>>;
normalize_ts(Ts0) ->
    case elib_type:is_numeric(Ts0) of
        true ->
            elib_dt:to_rfc3339(Ts0);
        false ->
            case elib_dt:rfc3339_to(Ts0, microsecond) of
                Ts when is_integer(Ts) ->
                    Ts0;
                _ ->
                    <<>>
            end
    end.

-spec keyword_like(binary()) -> binary().
keyword_like(<<>>) ->
    <<>>;
keyword_like(Keyword) ->
    <<"%", Keyword/binary, "%">>.

-spec uid_or_zero(integer()) -> integer().
uid_or_zero(Uid) when Uid > 0 ->
    Uid;
uid_or_zero(_) ->
    0.

-spec to_positive_int(binary()) -> integer().
to_positive_int(Val) when is_binary(Val) ->
    parse_id(Val).

-spec parse_id(term()) -> integer().
parse_id(Value) when is_integer(Value), Value > 0 ->
    Value;
parse_id(Value) when is_list(Value) ->
    parse_id(ec_cnv:to_binary(Value));
parse_id(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            ec_cnv:to_integer(Value);
        false ->
            case catch ec_cnv:to_integer(Value) of
                Id when is_integer(Id), Id > 0 -> Id;
                _ -> 0
            end
    end;
parse_id(_) ->
    0.

-spec to_lower_binary(binary()) -> binary().
to_lower_binary(Bin) ->
    list_to_binary(string:lowercase(ec_cnv:to_list(Bin))).

-spec ensure_message_audit_enabled(cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_message_audit_enabled(Req0) ->
    case imboy_policy:message_audit_enabled() of
        true ->
            ok;
        false ->
            feature_disabled(Req0)
    end.

-spec ensure_message_export_enabled(cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_message_export_enabled(Req0) ->
    case imboy_policy:message_export_enabled() of
        true ->
            ok;
        false ->
            feature_disabled(Req0)
    end.

-spec feature_disabled(cowboy_req:req()) -> {error, cowboy_req:req()}.
feature_disabled(Req0) ->
    {error,
     elib_response:error(
         Req0,
         imboy_error:error_msg(?ERR_FEATURE_DISABLED),
         ?ERR_FEATURE_DISABLED
     )}.

-spec sanitize_row_by_audit_mode(map(), none | metadata | full) -> map().
sanitize_row_by_audit_mode(Row, full) ->
    Row;
sanitize_row_by_audit_mode(Row, metadata) ->
    Row#{payload => <<>>};
sanitize_row_by_audit_mode(Row, none) ->
    Row#{payload => <<>>}.

-spec sanitize_rows_by_audit_mode([map()], none | metadata | full) -> [map()].
sanitize_rows_by_audit_mode(Rows, AuditMode) ->
    [sanitize_row_by_audit_mode(Row, AuditMode) || Row <- Rows].

%% Admin API 将 TSID 整数转为字符串，避免 JS 精度丢失。
-spec normalize_row(map()) -> map().
normalize_row(Row) ->
    CreatedAt = row_get(Row, <<"created_at">>, <<>>),
    ServerTs = row_get(Row, <<"server_ts">>, CreatedAt),
    FromId = elib_id:tsid_to_bin(row_get(Row, <<"from_id">>, 0)),
    ToId = elib_id:tsid_to_bin(row_get(Row, <<"to_id">>, 0)),
    #{
        scope => row_get(Row, <<"scope">>, <<>>),
        msg_id => row_get(Row, <<"msg_id">>, <<>>),
        from_id => FromId,
        to_id => ToId,
        msg_type => row_get(Row, <<"msg_type">>, <<>>),
        action => row_get(Row, <<"action">>, <<>>),
        payload => row_get(Row, <<"payload">>, <<>>),
        created_at => CreatedAt,
        server_ts => ServerTs
    }.

-spec row_get(map(), binary(), any()) -> any().
row_get(Row, Key, Default) ->
    case maps:find(Key, Row) of
        {ok, Value} ->
            Value;
        error ->
            AtomKey = binary_to_atom(Key, utf8),
            maps:get(AtomKey, Row, Default)
    end.

-spec get_count(map()) -> integer().
get_count(Row) ->
    case maps:find(<<"count">>, Row) of
        {ok, Val} -> ec_cnv:to_integer(Val);
        error ->
            case maps:find(count, Row) of
                {ok, Val2} -> ec_cnv:to_integer(Val2);
                error -> 0
            end
    end.
