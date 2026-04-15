-module(health_handler).

%%%
% 健康检查端点 / Health probes
% - /health, /health/live → Liveness：零依赖，恒返回 200（Docker HEALTHCHECK / K8s livenessProbe）
% - /health/ready         → Readiness：探测关键下游（DB 连接池），供 K8s readinessProbe / LB 剔除
%
% 设计原则 / Principles:
% - Liveness ≠ Readiness：liveness 失败重启容器；readiness 失败暂停收流量但不重启
% - Readiness DB 探测使用短超时，避免健康检查自身成为故障放大器
%%%

-behavior(cowboy_handler).

-export([init/2]).

%% 数据库探活超时（毫秒）/ DB probe timeout in milliseconds
-define(DB_PROBE_TIMEOUT_MS, 2000).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State) ->
    Action = maps:get(action, State, live),
    Req1 = case Action of
        live  -> reply_live(Req0);
        ready -> reply_ready(Req0)
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc Liveness：零依赖，恒定返回 200
-spec reply_live(cowboy_req:req()) -> cowboy_req:req().
reply_live(Req) ->
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                     <<"{\"status\":\"ok\"}">>,
                     Req).

%% @doc Readiness：探测关键下游；结构化 JSON 响应便于未来扩展多项检查
%% Structured JSON response (checks map) allows adding more probes later
%% without changing the response shape.
-spec reply_ready(cowboy_req:req()) -> cowboy_req:req().
reply_ready(Req) ->
    DbCheck = probe_db(),
    Checks = #{<<"db">> => check_to_binary(DbCheck)},
    case DbCheck of
        ok ->
            Body = iolist_to_binary(encode_ready(<<"ready">>, undefined, Checks)),
            cowboy_req:reply(200,
                             #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                             Body,
                             Req);
        {error, Reason} ->
            Body = iolist_to_binary(encode_ready(<<"not_ready">>, Reason, Checks)),
            cowboy_req:reply(503,
                             #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                             Body,
                             Req)
    end.

%% @doc 探测 DB：带超时的 SELECT 1
%% 连接池饱和（pooler 无可用成员）会被 elib_pg 转成 {error, no_connection}，
%% 这里据此区分 db_pool_saturated vs db_query_failed vs db_timeout / db_exception
-spec probe_db() -> ok | {error, atom()}.
probe_db() ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Result = try
            case elib_pg:query(<<"SELECT 1">>, []) of
                {ok, _} -> ok;
                {error, no_connection} -> {error, db_pool_saturated};
                _ -> {error, db_query_failed}
            end
        catch
            _:_ -> {error, db_exception}
        end,
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, R} -> R
    after ?DB_PROBE_TIMEOUT_MS ->
        exit(Pid, kill),
        {error, db_timeout}
    end.

%% @private 把 check 结果序列化为 binary 字符串（可读）
-spec check_to_binary(ok | {error, atom()}) -> binary().
check_to_binary(ok) -> <<"ok">>;
check_to_binary({error, Reason}) -> atom_to_binary(Reason, utf8).

%% @private 拼 readiness JSON（手写避免引入 jsone 到健康检查路径）
-spec encode_ready(binary(), atom() | undefined, map()) -> iodata().
encode_ready(Status, undefined, Checks) ->
    [<<"{\"status\":\"">>, Status, <<"\",\"checks\":">>, encode_checks(Checks), <<"}">>];
encode_ready(Status, Reason, Checks) ->
    [<<"{\"status\":\"">>, Status,
     <<"\",\"reason\":\"">>, atom_to_binary(Reason, utf8),
     <<"\",\"checks\":">>, encode_checks(Checks), <<"}">>].

-spec encode_checks(map()) -> iodata().
encode_checks(Checks) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        [[<<"\"">>, K, <<"\":\"">>, V, <<"\"">>] | Acc]
    end, [], Checks),
    [<<"{">>, lists:join(<<",">>, Pairs), <<"}">>].
