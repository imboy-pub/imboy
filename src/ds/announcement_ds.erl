-module(announcement_ds).

%%%
% 全局公告数据服务模块
% Global announcement data service module
%%%

-export([list/3, create/1, update/2, delete_by_id/1, publish/1, unpublish/1]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec list(integer(), integer(), map()) -> {ok, map()}.
list(Page, Size, Filters) ->
    Tb = announcement_repo:tablename(),
    Status = maps:get(status, Filters, undefined),
    Type = maps:get(type, Filters, undefined),
    Keyword = maps:get(keyword, Filters, undefined),

    Where0 = #{},
    Where1 =
        case Status of
            undefined -> Where0;
            S when is_integer(S) -> maps:put(status, S, Where0)
        end,
    Where2 =
        case Type of
            undefined -> Where1;
            T when is_binary(T), byte_size(T) > 0 -> maps:put(type, T, Where1)
        end,
    Where3 =
        case Keyword of
            undefined ->
                Where2;
            K when is_binary(K), byte_size(K) > 0 ->
                maps:put(
                    title, {op, <<"ILIKE">>, <<"%", (elib_pg:escape_like(K))/binary, "%">>}, Where2
                );
            _ ->
                Where2
        end,

    Column =
        <<"id, adm_user_id, title, type, status, pinned, published_at, expired_at, created_at, updated_at">>,
    {ok, P} = elib_pg:page_with_total(Tb, Column, Where3, <<"pinned desc, id desc">>, Page, Size),
    {ok, P}.

-spec create(map()) -> {ok, map()} | {error, binary()}.
create(Data) ->
    Tb = announcement_repo:tablename(),
    Title = maps:get(<<"title">>, Data, <<"">>),
    Body = maps:get(<<"body">>, Data, <<"">>),
    Type = maps:get(<<"type">>, Data, <<"info">>),
    AdmUserId = maps:get(<<"adm_user_id">>, Data, 0),
    Pinned = ec_cnv:to_integer(maps:get(<<"pinned">>, Data, 0)) =:= 1,
    ExpiredAt = maps:get(<<"expired_at">>, Data, undefined),

    case Title of
        <<>> ->
            {error, <<"标题不能为空"/utf8>>};
        _ ->
            Row = #{
                <<"adm_user_id">> => AdmUserId,
                <<"title">> => Title,
                <<"body">> => Body,
                <<"type">> => Type,
                <<"status">> => 0,
                <<"pinned">> => Pinned,
                <<"expired_at">> => ExpiredAt
            },
            case elib_pg:insert(Tb, Row) of
                {ok, Id} ->
                    {ok, #{<<"id">> => Id}};
                {error, Reason} ->
                    ?LOG_ERROR("announcement_ds:create error ~p", [Reason]),
                    {error, <<"创建公告失败"/utf8>>}
            end
    end.

-spec update(integer(), map()) -> {ok, map()} | {error, binary()}.
update(Id, Data) ->
    Tb = announcement_repo:tablename(),
    Title = maps:get(<<"title">>, Data, undefined),
    Body = maps:get(<<"body">>, Data, undefined),
    Type = maps:get(<<"type">>, Data, undefined),
    PinnedRaw = maps:get(<<"pinned">>, Data, undefined),
    Pinned =
        case PinnedRaw of
            undefined -> undefined;
            _ -> ec_cnv:to_integer(PinnedRaw) =:= 1
        end,
    ExpiredAt = maps:get(<<"expired_at">>, Data, undefined),

    Updates0 = #{<<"updated_at">> => elib_dt:now()},
    Updates1 =
        case Title of
            undefined -> Updates0;
            _ -> maps:put(<<"title">>, Title, Updates0)
        end,
    Updates2 =
        case Body of
            undefined -> Updates1;
            _ -> maps:put(<<"body">>, Body, Updates1)
        end,
    Updates3 =
        case Type of
            undefined -> Updates2;
            _ -> maps:put(<<"type">>, Type, Updates2)
        end,
    Updates4 =
        case Pinned of
            undefined -> Updates3;
            _ -> maps:put(<<"pinned">>, Pinned, Updates3)
        end,
    Updates5 =
        case ExpiredAt of
            undefined -> Updates4;
            _ -> maps:put(<<"expired_at">>, ExpiredAt, Updates4)
        end,

    case elib_pg:update(Tb, Updates5, <<"id = $1">>, [Id]) of
        {ok, _} ->
            {ok, #{<<"id">> => Id}};
        {error, Reason} ->
            ?LOG_ERROR("announcement_ds:update error ~p", [Reason]),
            {error, <<"更新公告失败"/utf8>>}
    end.

-spec delete_by_id(integer()) -> {ok, map()} | {error, binary()}.
delete_by_id(Id) ->
    Tb = announcement_repo:tablename(),
    case
        elib_pg:update(
            Tb, #{<<"status">> => -1, <<"updated_at">> => elib_dt:now()}, <<"id = $1">>, [Id]
        )
    of
        {ok, _} ->
            {ok, #{<<"id">> => Id}};
        {error, Reason} ->
            ?LOG_ERROR("announcement_ds:delete error ~p", [Reason]),
            {error, <<"删除公告失败"/utf8>>}
    end.

-spec publish(integer()) -> {ok, map()} | {error, binary()}.
publish(Id) ->
    Tb = announcement_repo:tablename(),
    case
        elib_pg:update(
            Tb,
            #{
                <<"status">> => 1,
                <<"published_at">> => elib_dt:now(),
                <<"updated_at">> => elib_dt:now()
            },
            <<"id = $1">>,
            [Id]
        )
    of
        {ok, _} ->
            {ok, #{<<"id">> => Id}};
        {error, Reason} ->
            ?LOG_ERROR("announcement_ds:publish error ~p", [Reason]),
            {error, <<"发布公告失败"/utf8>>}
    end.

-spec unpublish(integer()) -> {ok, map()} | {error, binary()}.
unpublish(Id) ->
    Tb = announcement_repo:tablename(),
    case
        elib_pg:update(Tb, #{<<"status">> => 2, <<"updated_at">> => elib_dt:now()}, <<"id = $1">>, [
            Id
        ])
    of
        {ok, _} ->
            {ok, #{<<"id">> => Id}};
        {error, Reason} ->
            ?LOG_ERROR("announcement_ds:unpublish error ~p", [Reason]),
            {error, <<"撤回公告失败"/utf8>>}
    end.
