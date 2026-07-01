-module(adm_logout_apply_handler).
-compile([nowarn_deprecated_catch]).
-dialyzer({nowarn_function, [normalize_ts/1]}).
%%%
% adm_logout_apply 控制器模块
% 注销申请审计 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

-define(EXPORT_CHUNK_SIZE, 1000).

-ifdef(EUNIT).
-export([normalize_ts/1, build_where_sql/1, csv_escape/1, row_to_csv_line/1]).
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
            export -> export(Method, Req0, State);
            reject -> reject(Method, Req0, State);
            approve -> approve(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    Filters = extract_filters(Req0),
    {WhereSql, Params} = build_where_sql(Filters),

    case user_log_ds:page_logout_apply_log(WhereSql, Params, Page, Size) of
        {ok, #{total := Total, list := Rows}} ->
            Items = [normalize_row(Row) || Row <- Rows],
            elib_response:success(Req0, #{
                list => Items,
                total => Total,
                page => Page,
                size => Size
            });
        {error, _Reason} ->
            elib_response:error(Req0, "查询失败")
    end;
list(_, Req0, _State) ->
    Req0.

%% @doc 驳回注销申请：将用户状态从 2（申请注销中）恢复为 1（正常）
-spec reject(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
reject(<<"POST">>, Req0, _State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsone:decode(Body, [{object_format, map}]),
    UidRaw = maps:get(<<"uid">>, Data, <<>>),
    Uid = parse_id(UidRaw),
    Reason = maps:get(<<"reason">>, Data, <<>>),
    case Uid > 0 of
        true ->
            case user_ds:reject_logout_apply(Uid) of
                {ok, [_]} ->
                    %% 记录驳回日志
                    ok = ?INFO_LOG([logout_apply_rejected, #{uid => Uid, reason => Reason}]),
                    elib_response:success(Req1, #{uid => Uid, status => <<"rejected">>});
                {ok, []} ->
                    elib_response:error(Req1, <<"用户不在注销申请状态"/utf8>>);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"操作失败"/utf8>>)
            end;
        false ->
            elib_response:error(Req1, <<"无效的用户ID"/utf8>>)
    end;
reject(_, Req0, _State) ->
    Req0.

%% @doc 审批通过注销申请：将用户状态设为 -1（已注销）
-spec approve(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
approve(<<"POST">>, Req0, _State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsone:decode(Body, [{object_format, map}]),
    UidRaw = maps:get(<<"uid">>, Data, <<>>),
    Uid = parse_id(UidRaw),
    case Uid > 0 of
        true ->
            case user_ds:approve_logout_apply(Uid) of
                {ok, [_]} ->
                    ok = ?INFO_LOG([logout_apply_approved, #{uid => Uid}]),
                    elib_response:success(Req1, #{uid => Uid, status => <<"approved">>});
                {ok, []} ->
                    elib_response:error(Req1, <<"用户不在注销申请状态"/utf8>>);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"操作失败"/utf8>>)
            end;
        false ->
            elib_response:error(Req1, <<"无效的用户ID"/utf8>>)
    end;
approve(_, Req0, _State) ->
    Req0.

-spec export(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
export(<<"GET">>, Req0, _State) ->
    Filters = extract_filters(Req0),
    {WhereSql, Params} = build_where_sql(Filters),
    Headers = #{
        <<"content-type">> => <<"text/csv; charset=utf-8">>,
        <<"content-disposition">> => <<"attachment; filename=\"logout_applications.csv\"">>,
        <<"cache-control">> => <<"no-store">>
    },
    Req1 = cowboy_req:stream_reply(200, Headers, Req0),
    ok = cowboy_req:stream_body(csv_header_with_bom(), nofin, Req1),
    stream_export_rows(Req1, WhereSql, Params, ?EXPORT_CHUNK_SIZE, 0);
export(_, Req0, _State) ->
    Req0.

-spec stream_export_rows(cowboy_req:req(), binary(), list(), pos_integer(), non_neg_integer()) ->
    ok.
stream_export_rows(Req, WhereSql, Params, Limit, Offset) ->
    case user_log_ds:list_logout_apply_log_chunk(WhereSql, Params, Limit, Offset) of
        {ok, []} ->
            cowboy_req:stream_body(<<>>, fin, Req);
        {ok, Rows} ->
            NormalizedRows = [normalize_row(Row) || Row <- Rows],
            CsvChunk = rows_to_csv_chunk(NormalizedRows),
            ok = cowboy_req:stream_body(CsvChunk, nofin, Req),
            case length(Rows) < Limit of
                true ->
                    cowboy_req:stream_body(<<>>, fin, Req);
                false ->
                    stream_export_rows(Req, WhereSql, Params, Limit, Offset + Limit)
            end;
        {error, _Reason} ->
            cowboy_req:stream_body(<<>>, fin, Req)
    end.

-spec extract_filters(cowboy_req:req()) -> map().
extract_filters(Req0) ->
    Uid = parse_uid_param(Req0),
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {ok, FromTs0} = elib_param:binary(from_ts, Req0, <<>>),
    {ok, ToTs0} = elib_param:binary(to_ts, Req0, <<>>),
    #{
        uid => uid_or_zero(Uid),
        keyword_like => keyword_like(Keyword),
        from_ts => normalize_ts(FromTs0),
        to_ts => normalize_ts(ToTs0)
    }.

-spec build_where_sql(map()) -> {binary(), list()}.
build_where_sql(Filters) ->
    Uid = maps:get(uid, Filters, 0),
    KeywordLike = maps:get(keyword_like, Filters, <<>>),
    FromTs = maps:get(from_ts, Filters, <<>>),
    ToTs = maps:get(to_ts, Filters, <<>>),
    {Idx1, Parts1, Params1} = maybe_add_uid(Uid > 0, Uid, 1, [], []),
    {Idx2, Parts2, Params2} = maybe_add_keyword(
        KeywordLike =/= <<>>, KeywordLike, Idx1, Parts1, Params1
    ),
    {Idx3, Parts3, Params3} = maybe_add_from_ts(FromTs =/= <<>>, FromTs, Idx2, Parts2, Params2),
    {_Idx4, Parts4, Params4} = maybe_add_to_ts(ToTs =/= <<>>, ToTs, Idx3, Parts3, Params3),
    {iolist_to_binary(Parts4), Params4}.

-spec maybe_add_uid(boolean(), integer(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_uid(false, _Uid, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_uid(true, Uid, Index, Parts, Params) ->
    Cond = <<" AND l.uid = $", (integer_to_binary(Index))/binary>>,
    {Index + 1, Parts ++ [Cond], Params ++ [Uid]}.

-spec maybe_add_keyword(boolean(), binary(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_keyword(false, _KeywordLike, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_keyword(true, KeywordLike, Index, Parts, Params) ->
    Pos = integer_to_binary(Index),
    Cond = <<" AND (u.account ILIKE $", Pos/binary, " OR u.nickname ILIKE $", Pos/binary, ")">>,
    {Index + 1, Parts ++ [Cond], Params ++ [KeywordLike]}.

-spec maybe_add_from_ts(boolean(), binary(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_from_ts(false, _FromTs, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_from_ts(true, FromTs, Index, Parts, Params) ->
    Cond = <<" AND l.created_at >= $", (integer_to_binary(Index))/binary, "::timestamptz">>,
    {Index + 1, Parts ++ [Cond], Params ++ [FromTs]}.

-spec maybe_add_to_ts(boolean(), binary(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_to_ts(false, _ToTs, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_to_ts(true, ToTs, Index, Parts, Params) ->
    Cond = <<" AND l.created_at <= $", (integer_to_binary(Index))/binary, "::timestamptz">>,
    {Index + 1, Parts ++ [Cond], Params ++ [ToTs]}.

-spec uid_or_zero(integer()) -> integer().
uid_or_zero(Uid) when Uid > 0 ->
    Uid;
uid_or_zero(_) ->
    0.

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

%% Admin API 将 TSID 整数转为字符串，避免 JS 精度丢失。
-spec normalize_row(map()) -> map().
normalize_row(Row) ->
    BodyBin = row_get(Row, <<"body">>, <<"{}">>),
    BodyMap = decode_body(BodyBin),
    Uid = elib_id:tsid_to_bin(row_get(Row, <<"uid">>, 0)),
    #{
        uid => Uid,
        account => row_get(Row, <<"account">>, <<>>),
        nickname => row_get(Row, <<"nickname">>, <<>>),
        user_status => row_get(Row, <<"user_status">>, 0),
        app_vsn => maps:get(<<"app_vsn">>, BodyMap, <<>>),
        did => maps:get(<<"did">>, BodyMap, <<>>),
        dtype => maps:get(<<"dtype">>, BodyMap, <<>>),
        ip => maps:get(<<"ip">>, BodyMap, <<>>),
        created_at => row_get(Row, <<"created_at">>, <<>>),
        body => BodyBin
    }.

-spec decode_body(binary()) -> map().
decode_body(BodyBin) ->
    try jsone:decode(BodyBin, [{object_format, map}]) of
        Decoded when is_map(Decoded) ->
            Decoded;
        _ ->
            #{}
    catch
        _:_ ->
            #{}
    end.

-spec row_get(map(), binary(), any()) -> any().
row_get(Row, Key, Default) ->
    case maps:find(Key, Row) of
        {ok, Value} ->
            Value;
        error ->
            AtomKey = binary_to_atom(Key, utf8),
            maps:get(AtomKey, Row, Default)
    end.

-spec csv_header_with_bom() -> binary().
csv_header_with_bom() ->
    <<239, 187, 191, "uid,account,nickname,app_vsn,did,dtype,ip,created_at,body\n">>.

-spec rows_to_csv_chunk([map()]) -> binary().
rows_to_csv_chunk(Rows) ->
    iolist_to_binary([[row_to_csv_line(Row), <<"\n">>] || Row <- Rows]).

-spec row_to_csv_line(map()) -> iolist().
row_to_csv_line(Row) ->
    Fields = [
        maps:get(uid, Row, 0),
        maps:get(account, Row, <<>>),
        maps:get(nickname, Row, <<>>),
        maps:get(app_vsn, Row, <<>>),
        maps:get(did, Row, <<>>),
        maps:get(dtype, Row, <<>>),
        maps:get(ip, Row, <<>>),
        maps:get(created_at, Row, <<>>),
        maps:get(body, Row, <<>>)
    ],
    lists:join(<<",">>, [csv_escape(Field) || Field <- Fields]).

-spec csv_escape(term()) -> binary().
csv_escape(Value) ->
    Bin =
        case Value of
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
