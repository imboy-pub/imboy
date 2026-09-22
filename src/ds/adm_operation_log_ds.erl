-module(adm_operation_log_ds).
-compile([nowarn_deprecated_catch]).
%%%
% adm_operation_log_ds 管理员操作审计日志数据服务层
% 封装 admin_operation_logs 表的写入与查询操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([insert/6]).
-export([insert_tx/7]).
-export([list/1]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 写入管理员操作审计日志
%% @param AdmUserId 管理员用户ID
%% @param Action 操作类型（binary），如 <<"force_logout">>
%% @param TargetId 操作对象ID（可传 0 表示无对象）
%% @param TargetType 对象类型（binary），如 <<"user">>
%% @param Detail 操作详情 map，将被序列化为 jsonb
%% @param Ip 操作者 IP（binary）
%% @return ok | {error, Reason}
-spec insert(
    integer(),
    binary(),
    integer() | undefined,
    binary() | undefined,
    map(),
    binary() | undefined
) -> ok | {error, term()}.
insert(AdmUserId, Action, TargetId, TargetType, Detail, Ip) ->
    {Sql, Params} = build_insert(AdmUserId, Action, TargetId, TargetType, Detail, Ip),
    case elib_pg:query(Sql, Params) of
        {ok, _} ->
            ok;
        {error, Reason} = Err ->
            ?ERROR_LOG(["adm_operation_log_ds:insert error: ", Reason]),
            Err
    end.

%% @doc **事务内**写入管理员操作审计日志（与业务写入共用同一 Conn）。
%%
%% 合同 EADM-01/C2（实施计划:109）：平台审计是 Organization 创建事务的**第 6 步**，
%% 且「失败必须整事务回滚」。因此这里**不做**任何错误吞没：返回 `{error, Reason}`
%% 交给调用方，由调用方 `throw({abort_tx, …})` 触发整事务 ROLLBACK。
%% 审计能静默丢失等于「创建了组织却没人知道是谁创建的」，属治理链路的完整性要求。
%%
%% @param Conn 由 elib_pg:with_tx 提供的事务连接
%% @return ok | {error, term()}
-spec insert_tx(
    term(),
    integer(),
    binary(),
    integer() | undefined,
    binary() | undefined,
    map(),
    binary() | undefined
) -> ok | {error, term()}.
insert_tx(Conn, AdmUserId, Action, TargetId, TargetType, Detail, Ip) ->
    {Sql, Params} = build_insert(AdmUserId, Action, TargetId, TargetType, Detail, Ip),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, _} ->
            ok;
        {error, Reason} = Err ->
            ?ERROR_LOG(["adm_operation_log_ds:insert_tx error: ", Reason]),
            Err
    end.

%% @doc 分页查询管理员操作日志
%% Opts 支持的键：adm_user_id（integer）、action（binary）、limit（integer）、offset（integer）
%% @return {ok, [map()]} | {error, Reason}
-spec list(map()) -> {ok, [map()]} | {error, term()}.
list(Opts) ->
    Limit = maps:get(limit, Opts, 20),
    Offset = maps:get(offset, Opts, 0),
    {WhereClauses, Params0} = build_where(Opts),
    WhereStr =
        case WhereClauses of
            [] ->
                <<>>;
            _ ->
                Parts = lists:join(<<" AND ">>, WhereClauses),
                iolist_to_binary([<<" WHERE ">>, Parts])
        end,
    ParamCount = length(Params0),
    LimitPos = integer_to_binary(ParamCount + 1),
    OffsetPos = integer_to_binary(ParamCount + 2),
    Sql = iolist_to_binary([
        <<"SELECT id, adm_user_id, action, target_id, target_type, detail, ip, created_at">>,
        <<" FROM admin_operation_logs">>,
        WhereStr,
        <<" ORDER BY created_at DESC">>,
        <<" LIMIT $">>,
        LimitPos,
        <<" OFFSET $">>,
        OffsetPos
    ]),
    elib_pg:query(Sql, Params0 ++ [Limit, Offset]).

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 构造审计 INSERT 的 SQL 与参数（insert/6 与 insert_tx/7 共用，
%% 保证两种写入路径的字段与归一化口径完全一致）。
-spec build_insert(
    integer(), binary(), integer() | undefined, binary() | undefined, map(), binary() | undefined
) -> {binary(), list()}.
build_insert(AdmUserId, Action, TargetId, TargetType, Detail, Ip) ->
    Id = elib_tsid:generate(admin_op_log),
    CreatedAt = elib_dt:millisecond(),
    TargetIdVal =
        case TargetId of
            V when is_integer(V), V > 0 -> V;
            _ -> null
        end,
    TargetTypeVal =
        case TargetType of
            B when is_binary(B), byte_size(B) > 0 -> B;
            _ -> null
        end,
    IpVal =
        case Ip of
            I when is_binary(I), byte_size(I) > 0 -> I;
            _ -> null
        end,
    DetailJson =
        case jsone_encode:encode(Detail, [native_utf8]) of
            {ok, Json} -> Json;
            _ -> <<"{}">>
        end,
    Sql = <<
        "INSERT INTO admin_operation_logs"
        " (id, adm_user_id, action, target_id, target_type, detail, ip, created_at)"
        " VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
    >>,
    {Sql, [Id, AdmUserId, Action, TargetIdVal, TargetTypeVal, DetailJson, IpVal, CreatedAt]}.

-spec build_where(map()) -> {[binary()], list()}.
build_where(Opts) ->
    lists:foldl(
        fun({Key, Col}, {Clauses, Params}) ->
            case maps:find(Key, Opts) of
                {ok, Val} when Val =/= undefined, Val =/= <<>> ->
                    Pos = integer_to_binary(length(Params) + 1),
                    Clause = iolist_to_binary([Col, <<" = $">>, Pos]),
                    {Clauses ++ [Clause], Params ++ [Val]};
                _ ->
                    {Clauses, Params}
            end
        end,
        {[], []},
        [
            {adm_user_id, <<"adm_user_id">>},
            {action, <<"action">>}
        ]
    ).
