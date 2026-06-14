-module(imboy_license).
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
-export([check_node_quota/0]).
-export([licensee/0, expires_at/0, info/0]).

-include("log.hrl").

-define(PT_KEY, {?MODULE, state}).
%% 社区版默认最大用户数（无 license 时），可被 sys.config {community_max_users, N} 覆盖
-define(DEFAULT_COMMUNITY_MAX_USERS, 100).
%% 到期宽限天数：过期后 N 天内仍按授权运行（仅告警），之后降级社区版
-define(GRACE_DAYS, 7).

%%===================================================================
%%% 启动加载
%%===================================================================

%% @doc 启动时加载并校验 license，结果存 persistent_term。
-spec load_and_validate() -> ok.
load_and_validate() ->
    State =
        case license_file_path() of
            undefined ->
                community_state(<<"未配置 license，按社区版运行"/utf8>>);
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
%% 供 imboy_cluster 在节点加入时调用（超限可拒绝加入或仅告警）。
-spec check_node_quota() -> ok | {error, node_quota_exceeded, integer(), integer()}.
check_node_quota() ->
    Max = max_nodes(),
    Current = length(nodes()) + 1,
    case Max =< 0 orelse Current =< Max of
        true -> ok;
        false -> {error, node_quota_exceeded, Current, Max}
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
