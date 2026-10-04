-module(enterprise_internal_error).

-moduledoc "/api/internal/v1/* 统一错误信封（EPGZ-02）—— 对应 manifest stable_error_codes。".
%%%
% enterprise_internal_error 是 /api/internal/v1/* 统一错误信封（EPGZ-02，
% plan-gz §6 / manifest stable_error_codes）。
%
% 合同（冻结）：
%   * 错误码只取 manifest stable_error_codes 的 16 个 snake_case 二进制，
%     不新造码；未知码按 internal_error 处理（fail-safe）。
%   * HTTP 状态映射固定：401 invalid_credential/credential_expired；
%     403 application_disabled/organization_disabled/insufficient_scope/
%     organization_boundary_violation；404 resource_not_found；
%     409 idempotency_conflict；422 identity_not_mapped；429 rate_limited；
%     503 security_gate_closed（限流配置缺失等 fail-closed 门）；
%     400 invalid_request；500 internal_error。
%   * 信封形态：{"error":{"code":"<stable>","message":"<generic>"}}——
%     message 是固定通用文案，不回显任何请求细节（redaction）。
%   * redaction 红线：credential/secret/Authorization 头/消息正文/
%     签名 URL 永不进入错误响应（error_body 只由 code 静态生成）。
%%%

-export([
    codes/0,
    http_status/1,
    message/1,
    error_body/1,
    reply/2,
    reply/3
]).

-include("log.hrl").

-define(STABLE_CODES, [
    <<"invalid_credential">>,
    <<"credential_expired">>,
    <<"application_disabled">>,
    <<"organization_disabled">>,
    <<"insufficient_scope">>,
    <<"resource_not_found">>,
    <<"identity_not_mapped">>,
    <<"organization_boundary_violation">>,
    <<"idempotency_conflict">>,
    <<"rate_limited">>,
    <<"security_gate_closed">>,
    <<"invalid_request">>,
    <<"internal_error">>,
    <<"version_conflict">>,
    <<"resource_conflict">>,
    <<"seat_limit_exceeded">>
]).

-define(STATUS_MAP, #{
    <<"invalid_credential">> => 401,
    <<"credential_expired">> => 401,
    <<"application_disabled">> => 403,
    <<"organization_disabled">> => 403,
    <<"insufficient_scope">> => 403,
    <<"resource_not_found">> => 404,
    <<"identity_not_mapped">> => 422,
    <<"organization_boundary_violation">> => 403,
    <<"idempotency_conflict">> => 409,
    <<"rate_limited">> => 429,
    <<"security_gate_closed">> => 503,
    <<"invalid_request">> => 400,
    <<"version_conflict">> => 409,
    <<"resource_conflict">> => 409,
    <<"seat_limit_exceeded">> => 409,
    <<"internal_error">> => 500
}).

-define(MESSAGES, #{
    <<"invalid_credential">> => <<"invalid credential">>,
    <<"credential_expired">> => <<"credential expired">>,
    <<"application_disabled">> => <<"application disabled">>,
    <<"organization_disabled">> => <<"organization disabled">>,
    <<"insufficient_scope">> => <<"insufficient scope">>,
    <<"resource_not_found">> => <<"resource not found">>,
    <<"identity_not_mapped">> => <<"identity not mapped">>,
    <<"organization_boundary_violation">> => <<"organization boundary violation">>,
    <<"idempotency_conflict">> => <<"idempotency key conflict">>,
    <<"rate_limited">> => <<"rate limited">>,
    <<"security_gate_closed">> => <<"security gate closed">>,
    <<"invalid_request">> => <<"invalid request">>,
    <<"version_conflict">> => <<"resource version conflict">>,
    <<"resource_conflict">> => <<"resource already exists">>,
    <<"seat_limit_exceeded">> => <<"seat limit exceeded">>,
    <<"internal_error">> => <<"internal error">>
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc stable 错误码全集（与 manifest stable_error_codes 逐字一致）。
-spec codes() -> [binary(), ...].
codes() ->
    ?STABLE_CODES.

%% @doc stable 码 → HTTP 状态；未知码 fail-safe 落 500（internal_error 语义）。
-spec http_status(binary()) -> pos_integer().
http_status(Code) ->
    case maps:find(Code, ?STATUS_MAP) of
        {ok, Status} -> Status;
        error -> 500
    end.

%% @doc stable 码 → 固定通用文案（不携带任何请求细节，天然 redacted）。
-spec message(binary()) -> binary().
message(Code) ->
    case maps:find(Code, ?MESSAGES) of
        {ok, Msg} -> Msg;
        error -> maps:get(<<"internal_error">>, ?MESSAGES)
    end.

%% @doc 错误信封 JSON 体：{"error":{"code":...,"message":...}}。
%% 只由 code 静态生成，永不掺入请求侧内容（credential/body 等）。
-spec error_body(binary()) -> binary().
error_body(Code) ->
    SafeCode =
        case lists:member(Code, ?STABLE_CODES) of
            true -> Code;
            false -> <<"internal_error">>
        end,
    jsone:encode(#{
        <<"error">> => #{
            <<"code">> => SafeCode,
            <<"message">> => message(SafeCode)
        }
    }).

%% @doc 中间件层错误应答（HTTP 真实状态码 + internal 信封 JSON）。
%% 只记 stable 码与路径，不记凭证/头值/正文（redaction 红线）。
-spec reply(cowboy_req:req(), binary()) -> cowboy_req:req().
reply(Req, Code) ->
    reply(Req, Code, []).

%% @doc reply/2 的追加头变体：信封体与状态码不变，仅附加响应头
%% （当前仅 429 rate_limited 的 Retry-After；ExtraHeaders 不得覆盖
%% content-type——调用方为仓内唯一适配器 enterprise_internal_middleware）。
-spec reply(cowboy_req:req(), binary(), [{binary(), binary()}]) ->
    cowboy_req:req().
reply(Req, Code, ExtraHeaders) ->
    ?WARN_LOG([
        enterprise_internal_rejected,
        #{
            code => Code,
            method => cowboy_req:method(Req),
            path => cowboy_req:path(Req)
        }
    ]),
    cowboy_req:reply(
        http_status(Code),
        maps:merge(
            #{<<"content-type">> => <<"application/json">>},
            maps:from_list(ExtraHeaders)
        ),
        error_body(Code),
        Req
    ).
