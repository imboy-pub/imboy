-module(imboy_license).
-compile([nowarn_deprecated_catch]).
%%%===================================================================
%%% @doc License 授权层 —— 规模/配额授权（锁规模上限，不锁功能）
%%%
%%% 商业模式：开源社区版功能完整但限规模（最大用户数/节点数）；购买 license
%%% 解锁更高规模上限 + edition 标记。锁的是"配额"而非"功能"，因此不违反
%%% 产品约束"社区版代码不得被植入按版次的残缺收费开关"。
%%%
%%% License 文件格式：
%%%   base64(payload_json) "." base64(rsa_sha256_signature)
%%%   payload: #{<<"edition">>, <<"max_users">>, <<"max_nodes">>,
%%%             <<"domains">>, <<"licensee">>,
%%%             <<"issued_at">>(ms), <<"expires_at">>(ms)}
%%%   max_users / max_nodes 为 0 表示不限量。
%%%
%%% 签名：Vendor 私钥签名 payload_json；部署端用随 release 打包的 Vendor 公钥
%%%       (priv/vendor_pubkey*.pem) 用 public_key:verify/4 验签。
%%%
%%% 加载：启动时 load_and_validate/0 → persistent_term 缓存；
%%%       运行时 is_valid/0 / limits/0 / check_user_quota/1 走缓存零 IO。
%%%
%%% 容错原则：无 license 文件 = 社区版（不报错）；签名/绑定/到期无效 = 记
%%%           WARN 后降级社区版（不阻断启动，避免 license 问题导致全站不可用）。
%%% @end
%%%===================================================================

-export([load_and_validate/0]).
-export([is_valid/0]).
-export([edition/0]).
-export([limits/0]).
-export([max_users/0]).
-export([max_nodes/0]).
-export([check_user_quota/1]).
-export([check_node_quota/0, check_node_quota/1]).
-export([licensee/0, expires_at/0, info/0, public_info/0]).
%% 供测试与内部：试用期判定纯函数 + 试用起始时间读取
-export([evaluate_trial/2, trial_start_ms/0]).

-include("log.hrl").

-define(PT_KEY, {?MODULE, state}).
%% 社区版默认最大用户数（无 license 时），可被 sys.config {community_max_users, N} 覆盖
-define(DEFAULT_COMMUNITY_MAX_USERS, 100).
%% 到期宽限天数：过期后 N 天内仍按授权运行（仅告警），之后降级社区版
-define(GRACE_DAYS, 7).
%% 无 license 文件时自动签发的专业版试用期天数（可被 sys.config {trial_days,N} 覆盖）
-define(DEFAULT_TRIAL_DAYS, 30).
%% 试用期配额（可被 sys.config 覆盖）
-define(DEFAULT_TRIAL_MAX_USERS, 500).
-define(DEFAULT_TRIAL_MAX_NODES, 3).

%%===================================================================
%%% 启动加载
%%===================================================================

%% @doc 启动时加载并校验 license，结果存 persistent_term。
-spec load_and_validate() -> ok.
load_and_validate() ->
    State =
        case license_file_path() of
            undefined ->
                trial_or_community_state();
            Path ->
                load_from_file(Path)
        end,
    persistent_term:put(?PT_KEY, State),
    log_state(State),
    ok.

-spec load_from_file(string()) -> map().
load_from_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            parse_and_verify(Bin);
        {error, Reason} ->
            community_state(
                iolist_to_binary(
                    io_lib:format("license 文件读取失败(~p)，降级社区版", [Reason])
                )
            )
    end.

-spec parse_and_verify(binary()) -> map().
parse_and_verify(Bin) ->
    case binary:split(string:trim(Bin), <<".">>) of
        [PayloadB64, SigB64] ->
            verify_payload(PayloadB64, SigB64);
        _ ->
            community_state(<<"license 格式错误，降级社区版"/utf8>>)
    end.

-spec verify_payload(binary(), binary()) -> map().
verify_payload(PayloadB64, SigB64) ->
    try
        PayloadJson = base64:decode(PayloadB64),
        Sig = base64:decode(SigB64),
        case verify_signature(PayloadJson, Sig) of
            true ->
                Payload = jsone:decode(PayloadJson, [{object_format, map}]),
                validate_claims(Payload);
            false ->
                community_state(<<"license 签名无效，降级社区版"/utf8>>)
        end
    catch
        _:_ ->
            community_state(<<"license 解析异常，降级社区版"/utf8>>)
    end.

%% @doc RSA-SHA256 验签（Vendor 公钥）
-spec verify_signature(binary(), binary()) -> boolean().
verify_signature(PayloadJson, Sig) ->
    case vendor_public_key() of
        {ok, PubKey} ->
            public_key:verify(PayloadJson, sha256, Sig, PubKey);
        error ->
            false
    end.

-spec vendor_public_key() -> {ok, term()} | error.
vendor_public_key() ->
    case file:read_file(vendor_pubkey_path()) of
        {ok, Pem} ->
            case public_key:pem_decode(Pem) of
                [Entry | _] -> {ok, public_key:pem_entry_decode(Entry)};
                _ -> error
            end;
        _ ->
            error
    end.

%% @doc 校验 license 声明：域名绑定 + 到期（含宽限期）
-spec validate_claims(map()) -> map().
validate_claims(Payload) ->
    Now = erlang:system_time(millisecond),
    ExpiresAt = to_int(maps:get(<<"expires_at">>, Payload, 0)),
    Domains = maps:get(<<"domains">>, Payload, []),
    Edition = maps:get(<<"edition">>, Payload, <<"community">>),
    case check_domain(Domains) of
        false ->
            community_state(<<"license 域名绑定不匹配，降级社区版"/utf8>>);
        true ->
            GraceMs = ?GRACE_DAYS * 86400000,
            if
                Now =< ExpiresAt ->
                    valid_state(Edition, Payload, valid);
                Now =< ExpiresAt + GraceMs ->
                    valid_state(Edition, Payload, grace);
                true ->
                    community_state(<<"license 已过期，降级社区版"/utf8>>)
            end
    end.

%% @doc 域名绑定校验：空列表=不绑定；host 未配置/通配=放行（宽松，避免误锁）
-spec check_domain(list()) -> boolean().
check_domain([]) ->
    true;
check_domain(Domains) when is_list(Domains) ->
    case elib_cnv:safe_to_binary(config_ds:env(host, <<>>)) of
        <<>> -> true;
        <<"_">> -> true;
        Host -> lists:member(Host, Domains)
    end;
check_domain(_) ->
    true.

-spec valid_state(binary(), map(), valid | grace) -> map().
valid_state(Edition, Payload, Status) ->
    #{
        valid => true,
        status => Status,
        edition => Edition,
        max_users => to_int(maps:get(<<"max_users">>, Payload, 0)),
        max_nodes => to_int(maps:get(<<"max_nodes">>, Payload, 1)),
        licensee => maps:get(<<"licensee">>, Payload, <<>>),
        expires_at => to_int(maps:get(<<"expires_at">>, Payload, 0)),
        reason => <<>>
    }.

-spec community_state(binary()) -> map().
community_state(Reason) ->
    #{
        valid => false,
        status => community,
        edition => <<"community">>,
        max_users => community_max_users(),
        max_nodes => 1,
        licensee => <<>>,
        expires_at => 0,
        reason => Reason
    }.

%% @doc 无 license 文件时：首次启动自动进入专业版试用期，记录起始时间到试用
%% 标记文件；试用期内按试用配额运行，过期后降级社区版。标记文件不可写时不阻断
%% 启动（降级社区版）。删除标记文件可重置试用——试用仅放宽配额、不锁功能，
%% 不做强防滥用（社区版本身免费且功能完整）。
-spec trial_or_community_state() -> map().
trial_or_community_state() ->
    case trial_start_ms() of
        {ok, StartMs} ->
            evaluate_trial(erlang:system_time(millisecond), StartMs);
        {error, _} ->
            community_state(<<"无 license 且试用标记不可用，按社区版运行"/utf8>>)
    end.

%% @doc 由当前时间与试用起始时间判定试用状态（纯函数，便于测试）。
%% 试用期内返回 trial 授权态，过期返回社区版态。
-spec evaluate_trial(integer(), integer()) -> map().
evaluate_trial(Now, StartMs) ->
    ExpiresAt = StartMs + trial_days() * 86400000,
    case Now =< ExpiresAt of
        true -> trial_state(ExpiresAt);
        false -> community_state(<<"试用期已结束，降级社区版"/utf8>>)
    end.

%% @doc 读取试用起始时间（毫秒）；标记文件不存在或内容损坏时写入当前时间并返回。
-spec trial_start_ms() -> {ok, integer()} | {error, term()}.
trial_start_ms() ->
    Path = trial_file_path(),
    case file:read_file(Path) of
        {ok, Bin} ->
            case catch binary_to_integer(string:trim(Bin)) of
                I when is_integer(I), I > 0 -> {ok, I};
                _ -> init_trial_file(Path)
            end;
        {error, enoent} ->
            init_trial_file(Path);
        {error, Reason} ->
            {error, Reason}
    end.

-spec init_trial_file(string()) -> {ok, integer()} | {error, term()}.
init_trial_file(Path) ->
    Now = erlang:system_time(millisecond),
    case file:write_file(Path, integer_to_binary(Now)) of
        ok -> {ok, Now};
        {error, Reason} -> {error, Reason}
    end.

-spec trial_state(integer()) -> map().
trial_state(ExpiresAt) ->
    #{
        valid => true,
        status => trial,
        edition => <<"trial">>,
        max_users => trial_max_users(),
        max_nodes => trial_max_nodes(),
        licensee => <<"试用"/utf8>>,
        expires_at => ExpiresAt,
        reason => <<>>
    }.

%%===================================================================
%%% 运行时查询（走 persistent_term 缓存，零 IO）
%%===================================================================

-spec state() -> map().
state() ->
    case persistent_term:get(?PT_KEY, undefined) of
        undefined -> community_state(<<"license 未加载"/utf8>>);
        S -> S
    end.

-spec is_valid() -> boolean().
is_valid() -> maps:get(valid, state(), false).

-spec edition() -> binary().
edition() -> maps:get(edition, state(), <<"community">>).

-spec limits() -> map().
limits() ->
    S = state(),
    #{max_users => maps:get(max_users, S, 0), max_nodes => maps:get(max_nodes, S, 1)}.

-spec max_users() -> integer().
max_users() -> maps:get(max_users, state(), 0).

-spec max_nodes() -> integer().
max_nodes() -> maps:get(max_nodes, state(), 1).

-spec licensee() -> binary().
licensee() -> maps:get(licensee, state(), <<>>).

-spec expires_at() -> integer().
expires_at() -> maps:get(expires_at, state(), 0).

-spec info() -> map().
info() -> state().

%% @doc 对外可见的脱敏授权状态：仅白名单字段。
%% 禁止外泄 license 原文、签名、私钥/公钥材料与内部降级原因（reason 可能含
%% 文件路径等部署细节）。所有对外 API/指标必须经此函数，不得直接透传 info/0。
-spec public_info() -> map().
public_info() ->
    S = state(),
    #{
        edition => maps:get(edition, S, <<"community">>),
        valid => maps:get(valid, S, false),
        status => atom_to_binary(maps:get(status, S, community), utf8),
        max_users => maps:get(max_users, S, 0),
        max_nodes => maps:get(max_nodes, S, 1),
        licensee => maps:get(licensee, S, <<>>),
        expires_at => maps:get(expires_at, S, 0)
    }.

%% @doc 规模 gate：当前用户数是否在 license 上限内。
%% max_users=0 表示不限量。CurrentCount >= Max 时拒绝。
-spec check_user_quota(integer()) -> ok | {error, quota_exceeded}.
check_user_quota(CurrentCount) when is_integer(CurrentCount) ->
    Max = max_users(),
    case Max =< 0 orelse CurrentCount < Max of
        true -> ok;
        false -> {error, quota_exceeded}
    end.

%% @doc 节点规模 gate：当前集群节点数是否在 license 上限内。
%% max_nodes=0 表示不限量；当前节点数 = 已连接节点数 + 本节点。
-spec check_node_quota() -> ok | {error, node_quota_exceeded, integer(), integer()}.
check_node_quota() ->
    check_node_quota(length(nodes()) + 1).

%% @doc 按给定节点数判定配额（纯函数，便于在非分布式环境下测试超限分支）。
%% 供 imboy_cluster 在节点加入前做「加入后是否超限」的前瞻判定。
-spec check_node_quota(integer()) -> ok | {error, node_quota_exceeded, integer(), integer()}.
check_node_quota(Count) when is_integer(Count) ->
    Max = max_nodes(),
    case Max =< 0 orelse Count =< Max of
        true -> ok;
        false -> {error, node_quota_exceeded, Count, Max}
    end.

%%===================================================================
%%% Internal
%%===================================================================

-spec license_file_path() -> string() | undefined.
license_file_path() ->
    case os:getenv("IMBOY_LICENSE_FILE") of
        Path when is_list(Path), Path =/= "" ->
            Path;
        _ ->
            Default = filename:join(code:priv_dir(imboy), "license.key"),
            case filelib:is_regular(Default) of
                true -> Default;
                false -> undefined
            end
    end.

-spec vendor_pubkey_path() -> string().
vendor_pubkey_path() ->
    case os:getenv("IMBOY_LICENSE_PUBKEY") of
        P when is_list(P), P =/= "" ->
            P;
        _ ->
            filename:join(code:priv_dir(imboy), "vendor_pubkey_dev.pem")
    end.

-spec community_max_users() -> integer().
community_max_users() ->
    application:get_env(imboy, community_max_users, ?DEFAULT_COMMUNITY_MAX_USERS).

-spec trial_days() -> integer().
trial_days() ->
    application:get_env(imboy, trial_days, ?DEFAULT_TRIAL_DAYS).

-spec trial_max_users() -> integer().
trial_max_users() ->
    application:get_env(imboy, trial_max_users, ?DEFAULT_TRIAL_MAX_USERS).

-spec trial_max_nodes() -> integer().
trial_max_nodes() ->
    application:get_env(imboy, trial_max_nodes, ?DEFAULT_TRIAL_MAX_NODES).

-spec trial_file_path() -> string().
trial_file_path() ->
    case os:getenv("IMBOY_TRIAL_FILE") of
        P when is_list(P), P =/= "" ->
            P;
        _ ->
            filename:join(code:priv_dir(imboy), "trial_start.epoch")
    end.

-spec to_int(term()) -> integer().
to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    case catch binary_to_integer(V) of
        I when is_integer(I) -> I;
        _ -> 0
    end;
to_int(_) ->
    0.

-spec log_state(map()) -> ok.
log_state(#{valid := true, edition := Ed, licensee := Lic, status := St}) ->
    ?INFO_LOG("[license] valid edition=~ts licensee=~ts status=~p", [Ed, Lic, St]);
log_state(#{reason := Reason}) ->
    ?WARN_LOG("[license] ~ts", [Reason]).
