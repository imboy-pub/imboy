-module(adm_group_helper).
%%% 群组管理共用工具函数 — 由 adm_group_handler 和 adm_group_sub_handler 共享
%%% 从 adm_group_handler.erl 提取 (2026-06-03)
-compile([nowarn_deprecated_catch]).

-define(ADM_GROUP_AUDIT_TYPE, 902).

-export([
    audit_group_governance/5,
    build_governance_log_where_sql/1,
    calc_total_pages/2,
    extract_governance_log_filters/1,
    list_group_files_with_total/5,
    list_tasks_with_total/5,
    list_user_categories_with_total/4,
    normalize_album_pk/1,
    normalize_assignment_pk/1,
    normalize_category_pk/1,
    normalize_file_pk/1,
    normalize_governance_log_row/1,
    normalize_notice_pk/1,
    normalize_notice_row/1,
    normalize_page_payload/3,
    normalize_positive_int/1,
    normalize_restore_task_pk/1,
    normalize_schedule_id/1,
    normalize_task_pk/1,
    normalize_user_pk/1,
    parse_gid_param/1,
    resolve_category_uid/2,
    resolve_task_audit_meta/1,
    resolve_vote_group_id/1,
    task_group_id_by_uid/1,
    task_is_deleted/1
]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

-spec extract_governance_log_filters(cowboy_req:req()) -> map().
extract_governance_log_filters(Req0) ->
    Uid = parse_positive_param(uid, Req0),
    GroupId = parse_positive_param(group_id, Req0),
    {ok, Action} = elib_param:binary(action, Req0, <<>>),
    {ok, TargetId} = elib_param:binary(target_id, Req0, <<>>),
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {ok, FromTs0} = elib_param:binary(from_ts, Req0, <<>>),
    {ok, ToTs0} = elib_param:binary(to_ts, Req0, <<>>),
    #{
        uid => uid_or_zero(Uid),
        group_id => uid_or_zero(GroupId),
        action => ec_cnv:to_binary(Action),
        target_id => ec_cnv:to_binary(TargetId),
        keyword_like => keyword_like(Keyword),
        from_ts => normalize_audit_ts(FromTs0),
        to_ts => normalize_audit_ts(ToTs0)
    }.

-spec build_governance_log_where_sql(map()) -> {binary(), list()}.
build_governance_log_where_sql(Filters) ->
    Uid = maps:get(uid, Filters, 0),
    GroupId = maps:get(group_id, Filters, 0),
    Action = maps:get(action, Filters, <<>>),
    TargetId = maps:get(target_id, Filters, <<>>),
    KeywordLike = maps:get(keyword_like, Filters, <<>>),
    FromTs = maps:get(from_ts, Filters, <<>>),
    ToTs = maps:get(to_ts, Filters, <<>>),
    {Idx1, Parts1, Params1} = maybe_add_uid(Uid > 0, Uid, 1, [], []),
    {Idx2, Parts2, Params2} = maybe_add_governance_keyword(
        KeywordLike =/= <<>>, KeywordLike, Idx1, Parts1, Params1
    ),
    {Idx3, Parts3, Params3} = maybe_add_governance_action(
        Action =/= <<>>, Action, Idx2, Parts2, Params2
    ),
    {Idx4, Parts4, Params4} = maybe_add_governance_group_id(
        GroupId > 0, GroupId, Idx3, Parts3, Params3
    ),
    {Idx5, Parts5, Params5} = maybe_add_governance_target_id(
        TargetId =/= <<>>, TargetId, Idx4, Parts4, Params4
    ),
    {Idx6, Parts6, Params6} = maybe_add_from_ts(FromTs =/= <<>>, FromTs, Idx5, Parts5, Params5),
    {_Idx7, Parts7, Params7} = maybe_add_to_ts(ToTs =/= <<>>, ToTs, Idx6, Parts6, Params6),
    {iolist_to_binary(Parts7), Params7}.

-spec maybe_add_uid(boolean(), integer(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_uid(false, _Uid, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_uid(true, Uid, Index, Parts, Params) ->
    Cond = <<" AND l.uid = $", (integer_to_binary(Index))/binary>>,
    {Index + 1, Parts ++ [Cond], Params ++ [Uid]}.

-spec maybe_add_governance_keyword(boolean(), binary(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_governance_keyword(false, _KeywordLike, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_governance_keyword(true, KeywordLike, Index, Parts, Params) ->
    Pos = integer_to_binary(Index),
    Cond = <<
        " AND (u.account ILIKE $",
        Pos/binary,
        " OR u.nickname ILIKE $",
        Pos/binary,
        " OR l.body ILIKE $",
        Pos/binary,
        ")"
    >>,
    {Index + 1, Parts ++ [Cond], Params ++ [KeywordLike]}.

-spec maybe_add_governance_action(boolean(), binary(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_governance_action(false, _Action, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_governance_action(true, Action, Index, Parts, Params) ->
    Pattern = <<"%\"action\":\"", Action/binary, "\"%">>,
    Cond = <<" AND l.body ILIKE $", (integer_to_binary(Index))/binary>>,
    {Index + 1, Parts ++ [Cond], Params ++ [Pattern]}.

-spec maybe_add_governance_group_id(boolean(), integer(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_governance_group_id(false, _GroupId, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_governance_group_id(true, GroupId, Index, Parts, Params) ->
    Pattern = <<"%\"group_id\":", (integer_to_binary(GroupId))/binary, "%">>,
    Cond = <<" AND l.body ILIKE $", (integer_to_binary(Index))/binary>>,
    {Index + 1, Parts ++ [Cond], Params ++ [Pattern]}.

-spec maybe_add_governance_target_id(boolean(), binary(), pos_integer(), [binary()], list()) ->
    {pos_integer(), [binary()], list()}.
maybe_add_governance_target_id(false, _TargetId, Index, Parts, Params) ->
    {Index, Parts, Params};
maybe_add_governance_target_id(true, TargetId, Index, Parts, Params) ->
    Pos1 = integer_to_binary(Index),
    Pos2 = integer_to_binary(Index + 1),
    PatternAsNumber = <<"%\"target_id\":", TargetId/binary, "%">>,
    PatternAsString = <<"%\"target_id\":\"", TargetId/binary, "\"%">>,
    Cond = <<" AND (l.body ILIKE $", Pos1/binary, " OR l.body ILIKE $", Pos2/binary, ")">>,
    {Index + 2, Parts ++ [Cond], Params ++ [PatternAsNumber, PatternAsString]}.

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
uid_or_zero(Value) when is_integer(Value), Value > 0 ->
    Value;
uid_or_zero(_) ->
    0.

-spec normalize_audit_ts(binary()) -> binary().
normalize_audit_ts(<<>>) ->
    <<>>;
normalize_audit_ts(Ts0) ->
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

-spec normalize_governance_log_row(map()) -> map().
normalize_governance_log_row(Row) ->
    BodyBin = row_value(Row, <<"body">>, <<"{}">>),
    BodyMap = decode_audit_body(BodyBin),
    #{
        uid => row_value(Row, <<"uid">>, 0),
        account => row_value(Row, <<"account">>, <<>>),
        nickname => row_value(Row, <<"nickname">>, <<>>),
        action => maps:get(<<"action">>, BodyMap, <<>>),
        operator_uid => maps:get(<<"operator_uid">>, BodyMap, 0),
        group_id => maps:get(<<"group_id">>, BodyMap, 0),
        target_id => maps:get(<<"target_id">>, BodyMap, <<>>),
        occurred_at => maps:get(<<"occurred_at">>, BodyMap, <<>>),
        extra => maps:get(<<"extra">>, BodyMap, #{}),
        created_at => row_value(Row, <<"created_at">>, <<>>),
        body => BodyBin
    }.

-spec decode_audit_body(binary()) -> map().
decode_audit_body(BodyBin) ->
    try jsone:decode(BodyBin, [{object_format, map}]) of
        Decoded when is_map(Decoded) ->
            Decoded;
        _ ->
            #{}
    catch
        _:_ ->
            #{}
    end.

-spec row_value(map(), binary(), any()) -> any().
row_value(Row, Key, Default) ->
    case maps:find(Key, Row) of
        {ok, Value} ->
            Value;
        error ->
            case maybe_existing_atom(Key) of
                undefined ->
                    Default;
                AtomKey ->
                    maps:get(AtomKey, Row, Default)
            end
    end.

-spec maybe_existing_atom(binary()) -> atom() | undefined.
maybe_existing_atom(Key) ->
    try binary_to_existing_atom(Key, utf8) of
        Atom when is_atom(Atom) ->
            Atom
    catch
        _:_ ->
            undefined
    end.

-spec list_tasks_with_total(integer(), integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
list_tasks_with_total(Gid, Status, Deleted, Page, Size) when Deleted =:= 1 ->
    case Status of
        S when is_integer(S), S >= 1, S =< 3 ->
            case group_task_ds:list_deleted_by_group_id(Gid, S, Page, Size) of
                {ok, List} ->
                    case group_task_ds:count_deleted_by_group_id(Gid, S) of
                        {ok, Total} ->
                            {ok, #{
                                list => List,
                                total => Total,
                                page => Page,
                                size => Size,
                                total_pages => calc_total_pages(Total, Size)
                            }};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            case group_task_ds:list_deleted_by_group_id(Gid, Page, Size) of
                {ok, List} ->
                    case group_task_ds:count_deleted_by_group_id(Gid) of
                        {ok, Total} ->
                            {ok, #{
                                list => List,
                                total => Total,
                                page => Page,
                                size => Size,
                                total_pages => calc_total_pages(Total, Size)
                            }};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end;
list_tasks_with_total(Gid, Status, _Deleted, Page, Size) ->
    case Status of
        S when is_integer(S), S >= 1, S =< 3 ->
            case group_task_ds:list_by_group_id(Gid, S, Page, Size) of
                {ok, List} ->
                    case group_task_ds:count_by_group_id(Gid, S) of
                        {ok, Total} ->
                            {ok, #{
                                list => List,
                                total => Total,
                                page => Page,
                                size => Size,
                                total_pages => calc_total_pages(Total, Size)
                            }};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            case group_task_ds:list_by_group_id(Gid, Page, Size) of
                {ok, List} ->
                    case group_task_ds:count_by_group_id(Gid) of
                        {ok, Total} ->
                            {ok, #{
                                list => List,
                                total => Total,
                                page => Page,
                                size => Size,
                                total_pages => calc_total_pages(Total, Size)
                            }};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end.

-spec calc_total_pages(non_neg_integer(), pos_integer()) -> non_neg_integer().
calc_total_pages(Total, Size) when is_integer(Total), is_integer(Size), Size > 0 ->
    case Total of
        0 -> 0;
        _ -> (Total + Size - 1) div Size
    end.

-spec normalize_page_payload(map(), integer(), integer()) -> map().
normalize_page_payload(Payload0, Page, Size) when is_map(Payload0) ->
    Items = maps:get(items, Payload0, maps:get(list, Payload0, [])),
    Total = maps:get(total, Payload0, length(Items)),
    #{
        list => Items,
        total => Total,
        page => maps:get(page, Payload0, Page),
        size => maps:get(size, Payload0, Size),
        total_pages => calc_total_pages(Total, maps:get(size, Payload0, Size))
    };
normalize_page_payload(_, Page, Size) ->
    #{
        list => [],
        total => 0,
        page => Page,
        size => Size,
        total_pages => 0
    }.

-spec list_user_categories_with_total(integer(), binary(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
list_user_categories_with_total(Uid, Keyword, Page, Size) ->
    case group_category_logic:list(Uid) of
        {ok, Categories0} when is_list(Categories0) ->
            Categories1 = [normalize_category_row(Item) || Item <- Categories0],
            Categories2 = filter_categories_by_keyword(Categories1, Keyword),
            Total = length(Categories2),
            Items = paginate_items(Categories2, Page, Size),
            {ok, #{
                list => Items,
                total => Total,
                page => Page,
                size => Size,
                total_pages => calc_total_pages(Total, Size)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec filter_categories_by_keyword(list(map()), binary()) -> list(map()).
filter_categories_by_keyword(Categories, Keyword) when Keyword =:= <<>> ->
    Categories;
filter_categories_by_keyword(Categories, Keyword) ->
    [Category || Category <- Categories, category_name_contains(Category, Keyword)].

-spec category_name_contains(map(), binary()) -> boolean().
category_name_contains(Category, Keyword) ->
    Name = ec_cnv:to_binary(maps:get(<<"category_name">>, Category, <<>>)),
    binary:match(Name, Keyword) =/= nomatch.

-spec paginate_items(list(), integer(), integer()) -> list().
paginate_items(Items, Page, Size) ->
    SafePage =
        case Page of
            P when is_integer(P), P > 0 -> P;
            _ -> 1
        end,
    SafeSize =
        case Size of
            S when is_integer(S), S > 0 -> S;
            _ -> 10
        end,
    Offset = (SafePage - 1) * SafeSize,
    lists:sublist(drop_items(Items, Offset), SafeSize).

-spec drop_items(list(), non_neg_integer()) -> list().
drop_items(Items, Offset) when Offset =< 0 ->
    Items;
drop_items([], _Offset) ->
    [];
drop_items([_Head | Tail], Offset) ->
    drop_items(Tail, Offset - 1).

-spec parse_gid_param(cowboy_req:req()) -> integer().
parse_gid_param(Req0) ->
    parse_positive_param(gid, Req0).

-spec parse_positive_param(atom(), cowboy_req:req()) -> integer().
parse_positive_param(Key, Req0) ->
    Value0 =
        case catch elib_param:binary(Key, Req0, <<>>) of
            {ok, Value} ->
                Value;
            _ ->
                case catch elib_param:int(Key, Req0, 0) of
                    {ok, Value} -> Value;
                    _ -> 0
                end
        end,
    normalize_positive_int(Value0).

-spec resolve_category_uid(term(), integer()) -> integer().
resolve_category_uid(UidRaw, Gid) ->
    case normalize_user_pk(UidRaw) of
        Uid when Uid > 0 ->
            Uid;
        _ when is_integer(Gid), Gid > 0 ->
            owner_uid_by_gid(Gid);
        _ ->
            0
    end.

-spec owner_uid_by_gid(integer()) -> integer().
owner_uid_by_gid(Gid) when is_integer(Gid), Gid > 0 ->
    case group_ds:find_by_id(Gid, <<"owner_uid">>) of
        #{<<"owner_uid">> := OwnerUid} when is_integer(OwnerUid), OwnerUid > 0 ->
            OwnerUid;
        _ ->
            0
    end;
owner_uid_by_gid(_) ->
    0.

-spec normalize_positive_int(term()) -> integer().
normalize_positive_int(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_positive_int(Value) when is_list(Value) ->
    normalize_positive_int(ec_cnv:to_binary(Value));
normalize_positive_int(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            ec_cnv:to_integer(Value);
        false ->
            case catch ec_cnv:to_integer(Value) of
                Id when is_integer(Id), Id > 0 -> Id;
                _ -> 0
            end
    end;
normalize_positive_int(_) ->
    0.

-spec normalize_user_pk(term()) -> integer().
normalize_user_pk(undefined) ->
    0;
normalize_user_pk(<<>>) ->
    0;
normalize_user_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_user_pk(Value) when is_list(Value) ->
    normalize_user_pk(ec_cnv:to_binary(Value));
normalize_user_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case ec_cnv:to_integer(Value) of
                        Id when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            0
                    end
            end
    end;
normalize_user_pk(_) ->
    0.

-spec normalize_category_row(map()) -> map().
normalize_category_row(Row) when is_map(Row) ->
    case maps:get(<<"id">>, Row, undefined) of
        Id when is_integer(Id), Id >= 0 ->
            Row#{<<"category_id">> => Id};
        _ ->
            Row
    end;
normalize_category_row(_Row) ->
    _Row.

-spec normalize_category_pk(term()) -> integer().
normalize_category_pk(undefined) ->
    0;
normalize_category_pk(<<>>) ->
    0;
normalize_category_pk(Value) when is_integer(Value), Value >= 0 ->
    Value;
normalize_category_pk(Value) when is_list(Value) ->
    normalize_category_pk(ec_cnv:to_binary(Value));
normalize_category_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case ec_cnv:to_integer(Value) of
                        Id when is_integer(Id), Id >= 0 ->
                            Id;
                        _ ->
                            0
                    end
            end
    end;
normalize_category_pk(_) ->
    0.

-spec list_group_files_with_total(integer(), binary(), binary(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
list_group_files_with_total(Gid, _Category, Keyword, Page, Size) when Keyword =/= <<>> ->
    case group_file_ds:search_by_name(Gid, Keyword, Page, Size) of
        {ok, List} ->
            Total = length(List),
            {ok, #{
                list => List,
                total => Total,
                page => Page,
                size => Size,
                total_pages => calc_total_pages(Total, Size)
            }};
        Error ->
            Error
    end;
list_group_files_with_total(Gid, Category, _Keyword, Page, Size) when Category =/= <<>> ->
    case group_file_ds:list_by_category(Gid, Category, Page, Size) of
        {ok, List} ->
            Total = length(List),
            {ok, #{
                list => List,
                total => Total,
                page => Page,
                size => Size,
                total_pages => calc_total_pages(Total, Size)
            }};
        Error ->
            Error
    end;
list_group_files_with_total(Gid, _Category, _Keyword, Page, Size) ->
    case group_file_ds:list_by_group(Gid, Page, Size, #{}) of
        {ok, List} ->
            case group_file_ds:count_by_group(Gid) of
                {ok, Total} ->
                    {ok, #{
                        list => List,
                        total => Total,
                        page => Page,
                        size => Size,
                        total_pages => calc_total_pages(Total, Size)
                    }};
                _ ->
                    Total = length(List),
                    {ok, #{
                        list => List,
                        total => Total,
                        page => Page,
                        size => Size,
                        total_pages => calc_total_pages(Total, Size)
                    }}
            end;
        Error ->
            Error
    end.

%% @doc 兼容 schedule_id:
%% - 原生 schedule_id (sched_xxx)
%% - 数字主键（int/数字字符串）
%% - ID 主键
-spec normalize_schedule_id(term()) -> binary() | undefined.
normalize_schedule_id(undefined) ->
    undefined;
normalize_schedule_id(<<>>) ->
    undefined;
normalize_schedule_id(Value) when is_integer(Value), Value > 0 ->
    schedule_id_by_pk(Value);
normalize_schedule_id(Value) when is_list(Value) ->
    normalize_schedule_id(ec_cnv:to_binary(Value));
normalize_schedule_id(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            undefined;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    schedule_id_by_pk(ec_cnv:to_integer(Value));
                false ->
                    case group_schedule_ds:find_by_schedule_id(Value) of
                        #{<<"schedule_id">> := _} ->
                            Value;
                        _ ->
                            case ec_cnv:to_integer(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    schedule_id_by_pk(Id);
                                _ ->
                                    undefined
                            end
                    end
            end
    end;
normalize_schedule_id(_) ->
    undefined.

-spec normalize_notice_pk(term()) -> integer().
normalize_notice_pk(undefined) ->
    0;
normalize_notice_pk(<<>>) ->
    0;
normalize_notice_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_notice_pk(Value) when is_list(Value) ->
    normalize_notice_pk(ec_cnv:to_binary(Value));
normalize_notice_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case ec_cnv:to_integer(Value) of
                        Id when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            0
                    end
            end
    end;
normalize_notice_pk(_) ->
    0.

-spec normalize_notice_row(map()) -> map().
normalize_notice_row(Notice) when is_map(Notice) ->
    case maps:get(<<"id">>, Notice, undefined) of
        Id when is_integer(Id), Id > 0 ->
            Notice#{<<"notice_id">> => Id};
        _ ->
            Notice
    end;
normalize_notice_row(_Row) ->
    _Row.

-spec normalize_file_pk(term()) -> integer().
normalize_file_pk(undefined) ->
    0;
normalize_file_pk(<<>>) ->
    0;
normalize_file_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_file_pk(Value) when is_list(Value) ->
    normalize_file_pk(ec_cnv:to_binary(Value));
normalize_file_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case group_file_ds:find_by_file_id(Value) of
                        #{<<"id">> := Id} when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            case ec_cnv:to_integer(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    Id;
                                _ ->
                                    0
                            end
                    end
            end
    end;
normalize_file_pk(_) ->
    0.

-spec normalize_album_pk(term()) -> integer().
normalize_album_pk(undefined) ->
    0;
normalize_album_pk(<<>>) ->
    0;
normalize_album_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_album_pk(Value) when is_list(Value) ->
    normalize_album_pk(ec_cnv:to_binary(Value));
normalize_album_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case group_album_ds:find_album_by_album_id(Value) of
                        #{<<"id">> := Id} when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            case ec_cnv:to_integer(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    Id;
                                _ ->
                                    0
                            end
                    end
            end
    end;
normalize_album_pk(_) ->
    0.

-spec schedule_id_by_pk(integer()) -> binary() | undefined.
schedule_id_by_pk(Id) when is_integer(Id), Id > 0 ->
    case group_schedule_ds:find_by_id(Id, <<"schedule_id">>) of
        #{<<"schedule_id">> := ScheduleId} when is_binary(ScheduleId), ScheduleId =/= <<>> ->
            ScheduleId;
        _ ->
            undefined
    end;
schedule_id_by_pk(_) ->
    undefined.

-spec normalize_task_pk(term()) -> integer().
normalize_task_pk(undefined) ->
    0;
normalize_task_pk(<<>>) ->
    0;
normalize_task_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_task_pk(Value) when is_list(Value) ->
    normalize_task_pk(ec_cnv:to_binary(Value));
normalize_task_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case group_task_ds:find_by_task_id(Value) of
                        {ok, #{<<"id">> := Id}} when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            case ec_cnv:to_integer(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    Id;
                                _ ->
                                    0
                            end
                    end
            end
    end;
normalize_task_pk(_) ->
    0.

-spec normalize_restore_task_pk(term()) -> integer().
normalize_restore_task_pk(undefined) ->
    0;
normalize_restore_task_pk(<<>>) ->
    0;
normalize_restore_task_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_restore_task_pk(Value) when is_list(Value) ->
    normalize_restore_task_pk(ec_cnv:to_binary(Value));
normalize_restore_task_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case group_task_ds:find_any_by_task_id(Value) of
                        {ok, #{<<"id">> := Id}} when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            case ec_cnv:to_integer(Value) of
                                Id when is_integer(Id), Id > 0 ->
                                    Id;
                                _ ->
                                    0
                            end
                    end
            end
    end;
normalize_restore_task_pk(_) ->
    0.

-spec normalize_assignment_pk(term()) -> integer().
normalize_assignment_pk(undefined) ->
    0;
normalize_assignment_pk(<<>>) ->
    0;
normalize_assignment_pk(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_assignment_pk(Value) when is_list(Value) ->
    normalize_assignment_pk(ec_cnv:to_binary(Value));
normalize_assignment_pk(Value) when is_binary(Value) ->
    case Value of
        <<>> ->
            0;
        _ ->
            case elib_type:is_numeric(Value) of
                true ->
                    ec_cnv:to_integer(Value);
                false ->
                    case ec_cnv:to_integer(Value) of
                        Id when is_integer(Id), Id > 0 ->
                            Id;
                        _ ->
                            0
                    end
            end
    end;
normalize_assignment_pk(_) ->
    0.

-spec task_is_deleted(map()) -> boolean().
task_is_deleted(Task) when is_map(Task) ->
    case maps:get(<<"deleted_at">>, Task, null) of
        undefined ->
            false;
        null ->
            false;
        <<>> ->
            false;
        _ ->
            true
    end;
task_is_deleted(_) ->
    false.

-spec resolve_vote_group_id(binary()) -> integer().
resolve_vote_group_id(VoteId) when is_binary(VoteId), VoteId =/= <<>> ->
    case group_vote_ds:find_by_vote_id(VoteId) of
        {ok, Vote} ->
            maps:get(<<"group_id">>, Vote, 0);
        _ ->
            0
    end;
resolve_vote_group_id(_) ->
    0.

-spec resolve_task_audit_meta(integer()) -> {integer(), binary()}.
resolve_task_audit_meta(TaskPk) when is_integer(TaskPk), TaskPk > 0 ->
    case group_task_ds:find_by_id(TaskPk) of
        {ok, Task} ->
            {
                maps:get(<<"group_id">>, Task, 0),
                maps:get(<<"task_id">>, Task, <<>>)
            };
        _ ->
            {0, <<>>}
    end;
resolve_task_audit_meta(_) ->
    {0, <<>>}.

-spec task_group_id_by_uid(binary()) -> integer().
task_group_id_by_uid(TaskUid) when is_binary(TaskUid), TaskUid =/= <<>> ->
    case group_task_ds:find_by_task_id(TaskUid) of
        {ok, Task} ->
            maps:get(<<"group_id">>, Task, 0);
        _ ->
            0
    end;
task_group_id_by_uid(_) ->
    0.

-spec audit_group_governance(integer(), integer(), binary(), term(), map()) -> ok.
audit_group_governance(AdmUserId, _GroupId, _Action, _TargetId, _Extra) when
    not is_integer(AdmUserId); AdmUserId =< 0
->
    ok;
audit_group_governance(AdmUserId, GroupId, Action, TargetId, Extra) ->
    Now = elib_dt:now(),
    AuditBody = #{
        <<"action">> => Action,
        <<"operator_uid">> => AdmUserId,
        <<"group_id">> => GroupId,
        <<"target_id">> => TargetId,
        <<"occurred_at">> => Now,
        <<"extra">> => Extra
    },
    try
        _ = user_log_ds:add(#{
            type => ?ADM_GROUP_AUDIT_TYPE,
            uid => AdmUserId,
            body => jsone:encode(AuditBody, [native_utf8]),
            remark => <<"adm_group_governance">>,
            created_at => Now
        }),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?DEBUG_LOG("群治理审计日志写入失败: ~p", [{Class, Reason, Stacktrace}]),
            ok
    end.
