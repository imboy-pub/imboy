-module(eb_asset_download_ticket).
-moduledoc "EB 域下载票据（V3-17）：purpose-derived key，承载 ACL 重放所需的最小声明。".
%%%
%%% 企业/客服 content 读链（G17）：
%%%   * HTTP 端点完成原鉴权后签发本票据，302 到边缘 `/edge?ticket=…`；
%%%   * 边缘每个字节请求（含 Range 分片）回调 authorize，主 BEAM 用本票据
%%%     **重放完整 EB ACL**（org/workspace/asset/绑定门/成员或访客经办）——
%%%     逐请求授权，撤权即时生效；
%%%   * 密钥与主域 attachment_download_ticket 同源不同 purpose 派生
%%%     （HMAC-SHA256 purpose separation），互不接受对方 sub；
%%%   * claims 全部是**服务端派生事实**（鉴权时已有），客户端无法申报扩权。
%%%

-export([issue/2, verify/1, verify/2, subject/0]).

-define(TTL, 300).

subject() ->
    <<"eb_asset_download">>.

%% Claims 必填：org_id / workspace_id / asset_id / actor_kind(member|contact) /
%% actor_id；可选：conversation_id（widget 绑定门重放；member/platform 面无）、
%% visitor_session_id（仅审计投影，不参与鉴权判定）。
-spec issue(map(), binary()) -> {ok, binary()} | {error, invalid}.
issue(Claims, Secret) when
    is_map(Claims),
    is_binary(Secret),
    byte_size(Secret) > 0
->
    case validate(Claims) of
        ok ->
            Now = erlang:system_time(second),
            Payload0 = #{
                <<"sub">> => subject(),
                <<"iat">> => Now,
                <<"exp">> => Now + ?TTL,
                <<"org_id">> => maps:get(org_id, Claims),
                <<"workspace_id">> => maps:get(workspace_id, Claims),
                <<"asset_id">> => maps:get(asset_id, Claims),
                <<"actor_kind">> => actor_kind_bin(maps:get(actor_kind, Claims)),
                <<"actor_id">> => maps:get(actor_id, Claims)
            },
            Payload = with_optional(Payload0, Claims, [conversation_id, visitor_session_id]),
            {ok, imboy_jwt:sign(Payload, key(Secret))};
        {error, _} = Err ->
            Err
    end;
issue(_, _) ->
    {error, invalid}.

%% verify/1 用实例密钥（edge authorize 回调侧；与 issue 同源 config jwt_key）。
-spec verify(binary()) -> {ok, map()} | {error, invalid}.
verify(Ticket) ->
    verify(Ticket, config_ds:env(jwt_key, <<>>)).

-spec verify(binary(), binary()) -> {ok, map()} | {error, invalid}.
verify(Ticket, Secret) when
    is_binary(Ticket),
    byte_size(Ticket) =< 8192,
    is_binary(Secret),
    byte_size(Secret) > 0
->
    case imboy_jwt:verify(Ticket, key(Secret)) of
        {ok,
            #{
                <<"sub">> := <<"eb_asset_download">>,
                <<"org_id">> := OrgId,
                <<"workspace_id">> := WsId,
                <<"asset_id">> := AssetId,
                <<"actor_kind">> := KindBin,
                <<"actor_id">> := ActorId,
                <<"iat">> := Issued,
                <<"exp">> := Expires
            } = Verified} when
            is_integer(OrgId),
            is_integer(WsId),
            is_integer(AssetId),
            is_integer(ActorId),
            is_integer(Issued),
            is_integer(Expires),
            Expires > Issued,
            Expires =< Issued + ?TTL
        ->
            case kind_atom(KindBin) of
                {ok, Kind} ->
                    %% 可选 claims（conversation_id/visitor_session_id）直接从
                    %% 验签后的完整 claims map 恢复——绑定门重放需逐字比对。
                    Claims = #{
                        org_id => OrgId,
                        workspace_id => WsId,
                        asset_id => AssetId,
                        actor_kind => Kind,
                        actor_id => ActorId
                    },
                    {ok, take_optional(Claims, Verified)};
                error ->
                    {error, invalid}
            end;
        _ ->
            {error, invalid}
    end;
verify(_, _) ->
    {error, invalid}.

validate(Claims) ->
    IntKeys = [org_id, workspace_id, asset_id, actor_id],
    IntsOk = lists:all(
        fun(K) ->
            V = maps:get(K, Claims, undefined),
            is_integer(V) andalso V > 0
        end,
        IntKeys
    ),
    case {IntsOk, maps:get(actor_kind, Claims, undefined)} of
        {true, member} -> ok;
        {true, contact} -> ok;
        _ -> {error, invalid_claims}
    end.

actor_kind_bin(member) -> <<"member">>;
actor_kind_bin(contact) -> <<"contact">>.

kind_atom(<<"member">>) -> {ok, member};
kind_atom(<<"contact">>) -> {ok, contact};
kind_atom(_) -> error.

with_optional(Payload, Claims, Keys) ->
    copy_optional(Payload, Claims, [{K, K} || K <- Keys]).

take_optional(Claims, Verified) ->
    %% 输出统一 atom 键（与 edge_authorize_content 的 claims 模式及
    %% content_stream Params 约定一致）；JWT 层的 binary 键在此转换。
    copy_optional(Claims, Verified, [
        {<<"conversation_id">>, conversation_id},
        {<<"visitor_session_id">>, visitor_session_id}
    ]).

copy_optional(Target, Source, Keys) ->
    lists:foldl(
        fun({FromKey, ToKey}, Acc) ->
            case maps:get(FromKey, Source, undefined) of
                undefined -> Acc;
                V -> Acc#{ToKey => V}
            end
        end,
        Target,
        Keys
    ).

key(Secret) ->
    crypto:mac(hmac, sha256, Secret, <<"imboy:eb-asset-download:v1">>).
