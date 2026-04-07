-module(report_logic).

%% Stable ops_governance report domain boundary.

-export([create/5]).
-export([admin_list/5]).
-export([admin_resolve/5]).
-export([admin_batch_resolve/5]).

-include("error_code.hrl").
-include("log.hrl").

-spec create(integer(), term(), term(), term(), term()) -> {ok, map()} | {error, binary()}.
create(ReporterUid, TargetTypeRaw, TargetIdRaw, ReasonRaw, DescRaw) when is_integer(ReporterUid), ReporterUid > 0 ->
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
            case report_ticket_repo:create(Type, TargetId, ReporterUid, Reason, Desc) of
                {ok, ReportId} ->
                    {ok, #{
                        <<"report_id">> => safe_encode(ReportId),
                        <<"target_type">> => Type
                    }};
                {error, _Reason} ->
                    {error, <<"举报失败"/utf8>>}
            end
    end;
create(_, _, _, _, _) ->
    {error, <<"举报参数无效"/utf8>>}.

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
            case report_ticket_repo:page_admin(RepoFilter, Page2, Size2) of
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
admin_resolve(AdmUid, TargetTypeRaw, ReportIdRaw, Result, NoteRaw) when is_integer(AdmUid), AdmUid > 0 ->
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

-spec admin_batch_resolve(integer(), term(), list(), integer(), term()) -> {ok, map()} | {error, binary()}.
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

-spec resolve_non_moment(integer(), binary(), term(), integer(), binary()) -> ok | {error, binary()}.
resolve_non_moment(AdmUid, TargetType, ReportIdRaw, Result, Note) ->
    ReportId = decode_positive_id(ReportIdRaw),
    case ReportId > 0 of
        false ->
            {error, <<"举报参数无效"/utf8>>};
        true ->
            case report_ticket_repo:find_by_id(ReportId) of
                Row when is_map(Row), map_size(Row) > 0 ->
                    StoredType = normalize_target_type(maps:get(<<"target_type">>, Row, <<>>), undefined),
                    case TargetType =:= <<>> orelse TargetType =:= StoredType of
                        false ->
                            {error, <<"举报类型不匹配"/utf8>>};
                        true ->
                            case report_ticket_repo:resolve(ReportId, Result, Note, AdmUid) of
                                {ok, N} when N > 0 ->
                                    case report_action_log_repo:create(ReportId, AdmUid, Result, Note) of
                                        {ok, _} -> ok;
                                        {error, LogReason} ->
                                            ?ERROR_LOG(["report_action_log_failed", ReportId, AdmUid, LogReason])
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
        <<"handled_by">> => safe_encode(HandledBy)
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
        _ -> Default
    end.

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
