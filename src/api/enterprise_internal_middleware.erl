-module(enterprise_internal_middleware).

%%%
% enterprise_internal_middleware 是 /api/internal/v1/* 的 Cowboy 中间件壳
% （EPGZ-02）。决策逻辑全部在 enterprise_internal_auth:decide/4（可注入
% AuthFun 纯测试）；本模块只做两件事：
%   1. 从 cowboy 请求取 method/path/headers，注入池化认证闭包；
%   2. 把 decide 的结果映射为 Env 注入（成功）或错误信封应答（失败）。
%
% W4 接线（A0）：auth_middleware 对 <<"/api/internal/v1/", _/binary>> 前缀
% 委托本模块 execute/2（不得进入人类 JWT/签名门，也不得被 open() 放行）；
% is_internal_path/1 供前缀判定复用。
%
% 成功时认证产物写入 Env 两处（handler 侧任取其一）：
%   env.enterprise_internal_ctx  —— 中间件层
%   env.handler_opts.enterprise_internal —— 当 handler_opts 是 map 时合并
%
% W4 增补（A0）：路径绑定合并进 handler_opts。cowboy_router 排在本中间件之前，
% 路由命中后 bindings 已在 Env 内；worker 侧壳按 maps:get(group_id, State, 0)
% / maps:get(delivery_id, State, undefined) 读取绑定（INT-05/06 的 {group_id}、
% INT-13 的 {delivery_id}），因此这里必须把 bindings 一并注入，否则路径参数
% 恒为缺省值——group_id 落 0 → invalid_request，delivery_id 落 undefined。
%%%

-behaviour(cowboy_middleware).

-export([execute/2, is_internal_path/1, normalize_code/1]).

-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc internal 面前缀判定（manifest allowed_prefix）。
-spec is_internal_path(binary()) -> boolean().
is_internal_path(<<"/api/internal/v1/", _/binary>>) ->
    true;
is_internal_path(_Path) ->
    false.

%% @doc Cowboy 中间件入口：认证链编排（见 enterprise_internal_auth:decide/4）。
%% 认证失败/任一前置失败 → 稳定错误信封 + {stop, Req}。
-spec execute(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    AuthFun = pool_auth_fun(Headers),
    case enterprise_internal_auth:decide(Method, Path, Headers, AuthFun) of
        {ok, Ctx} ->
            {ok, Req, inject_ctx(Req, Env, Ctx)};
        {error, Code} ->
            BinCode = normalize_code(Code),
            {stop, enterprise_internal_error:reply(Req, BinCode, extra_headers(BinCode))}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 稳定码 → 追加响应头。当前仅 429 rate_limited 附 Retry-After
%% （delta-seconds）：值由 rate_gate 暂存、take_retry_after_seconds/0 一次性
%% 取出即清（见该函数注释）；未限流/无值时不带头，信封体不变。
-spec extra_headers(binary()) -> [{binary(), binary()}].
extra_headers(<<"rate_limited">>) ->
    case enterprise_internal_auth:take_retry_after_seconds() of
        Sec when is_integer(Sec), Sec > 0 ->
            [{<<"retry-after">>, integer_to_binary(Sec)}];
        _ ->
            []
    end;
extra_headers(_Code) ->
    [].

%% 池化认证闭包：parse + 单事务认证链（格式错误在此归一 invalid_credential）。
%% 返回码沿用 enterprise_internal_auth 的内部 **atom** 约定，由本模块的
%% normalize_code/1 在出信封前统一转 manifest 二进制码。
-spec pool_auth_fun(map()) -> fun(() -> {ok, map()} | {error, atom()}).
pool_auth_fun(Headers) ->
    fun() ->
        case
            enterprise_internal_auth:parse_bearer(
                maps:get(<<"authorization">>, Headers, undefined)
            )
        of
            {error, _Code} ->
                {error, invalid_credential};
            {ok, Prefix, Secret} ->
                enterprise_internal_auth:authenticate(Prefix, Secret)
        end
    end.

%% 出信封前的码归一（本模块是 internal 面的 HTTP 适配器，归一必须**恰好一次**）。
%%
%% enterprise_internal_auth 的 decide/4 与其 gate 按内部 atom 约定返回错误码
%% （parse_bearer/1、authenticate_tx/* 同款，A2 冻结测试按 atom 断言），而
%% enterprise_internal_error:reply/2 只认 manifest 的 13 个 snake_case 二进制码，
%% 未知码一律 fail-safe 成 internal_error + HTTP 500。两者直接相接会把**所有**
%% 业务拒绝码（401/403/404/409/422/429/503）压成 500，且响应体不含真实码——
%% W4 用真 cowboy 请求逐个 INT 路径实测暴露（A2 单测直调 decide 断言 atom，
%% 不经信封，故未发现）。此处只做形态归一，不改任何语义/映射。
-spec normalize_code(atom() | binary()) -> binary().
normalize_code(Code) when is_atom(Code) -> atom_to_binary(Code, utf8);
normalize_code(Code) when is_binary(Code) -> Code.

%% 认证产物 + 路径绑定注入：上下文进 Env#handler_opts，
%% 路径绑定（{:group_id}/{:delivery_id}）从 **Req** 取——cowboy_router 把
%% bindings 放进 Req，handler/handler_opts 才放进 Env（cowboy_router.erl
%% 命中分支的返回值），所以两者来源不同，不能都从 Env 读。
-spec inject_ctx(cowboy_req:req(), map(), map()) -> map().
inject_ctx(Req, Env, Ctx0) ->
    %% INT-BE-03：认证产物 ctx 携带请求关联 ID，供业务侧审计行
    %% （enterprise_audit_event.detail.correlation_id）做请求级归因——
    %% mutation 优先用 Idempotency-Key（审计行与幂等行可互查）；无幂等键的
    %% 请求（GET / INT-14 single_use_code）用一次性随机串，保证审计行恒有
    %% correlation 字段。该值只进审计/日志 detail，不参与任何鉴权判定。
    Ctx = Ctx0#{correlation_id => correlation_id(Req)},
    Env1 = Env#{enterprise_internal_ctx => Ctx},
    case maps:find(handler_opts, Env) of
        {ok, Opts} when is_map(Opts) ->
            Merged = maps:merge(Opts, bindings(Req)),
            Env1#{handler_opts := Merged#{enterprise_internal => Ctx}};
        _ ->
            Env1
    end.

%% 请求关联 ID：Idempotency-Key 优先，缺失生成 128-bit 随机 hex。
-spec correlation_id(cowboy_req:req()) -> binary().
correlation_id(Req) ->
    case cowboy_req:header(<<"idempotency-key">>, Req) of
        K when is_binary(K), K =/= <<>> -> K;
        _ ->
            iolist_to_binary([
                io_lib:format("~64.16.0b", [R])
             || R <- [binary:decode_unsigned(crypto:strong_rand_bytes(8))]
            ])
    end.

%% 路径绑定 → handler_opts 扁平合并。cowboy_router 恒为首个中间件，命中路由后
%% bindings 必在 Req 内；缺失（未命中路由，本中间件不会被调用）退化为空 map。
-spec bindings(cowboy_req:req()) -> map().
bindings(Req) ->
    case maps:get(bindings, Req, undefined) of
        B when is_map(B) -> maps:map(fun normalize_binding/2, B);
        _ -> #{}
    end.

%% cowboy 的路径段恒为 binary；{group_id} / {delivery_id} 是 TSID，若原样
%% 下传，worker 侧壳 maps:get(group_id, State, 0) 拿到 binary 会绕过默认值
%% 直接进 logic → 被当成非法入参。这里按**已知 TSID 段名**收敛为整数
%% （只白名单两个名字，其余绑定原样保留，不做通用数字猜测）。
-spec normalize_binding(atom(), binary()) -> integer() | binary().
normalize_binding(Key, Value) when is_binary(Value) ->
    case Key of
        group_id -> to_tsid(Value);
        delivery_id -> to_tsid(Value);
        _ -> Value
    end;
normalize_binding(_Key, Value) ->
    Value.

-spec to_tsid(binary()) -> integer() | binary().
to_tsid(Value) ->
    case is_all_digits(Value) of
        true ->
            try
                binary_to_integer(Value)
            catch
                _:_ -> Value
            end;
        false ->
            Value
    end.

-spec is_all_digits(binary()) -> boolean().
is_all_digits(<<>>) ->
    false;
is_all_digits(Bin) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Bin)).
