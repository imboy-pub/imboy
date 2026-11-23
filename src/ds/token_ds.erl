-module(token_ds).

-moduledoc "token 领域服务 / token domain service。".
%%%
% token_ds 是 token domain service 缩写
%%%

-export([encrypt_token/1, encrypt_token/2, encrypt_seat_token/2, encrypt_web_token/4]).
-export([encrypt_web_seat_token/4, decrypt_seat_token/1]).
-export([encrypt_refreshtoken/1, encrypt_refreshtoken/2]).
-export([decrypt_token/1, decrypt_web_token/2]).
-export([issue_device_session/3, issue_web_session/4]).

% -export ([get_uid/1]).

-include("common.hrl").
-include("log.hrl").

%% @doc 生成refresh token
%% 生成用于刷新访问令牌的长效令牌，有效期由?REFRESHTOKEN_VALID定义。
%% @param ID 用户ID或标识符
%% @returns 编码后的JWT refresh token
% token_ds:decrypt_token(token_ds:encrypt_refreshtoken(1)).
-spec encrypt_refreshtoken(integer() | binary()) -> binary().
encrypt_refreshtoken(ID) ->
    encrypt_refreshtoken(ID, <<>>).

%% @doc 生成绑定设备 DID 的 refresh token（E2EE-013）。
-spec encrypt_refreshtoken(integer() | binary(), binary()) -> binary().
encrypt_refreshtoken(ID, Did) ->
    do_encrypt_token(ID, Did, ?REFRESHTOKEN_VALID, <<"rtk">>).

%% @doc 生成访问token
%% 生成用于用户认证的访问令牌，有效期由?TOKEN_VALID定义。
%% 使用HS256算法和配置的JWT密钥进行签名。
%%
%% @param ID 用户ID或标识符
%% @returns 编码后的JWT access token
-spec encrypt_token(integer() | binary()) -> binary().
encrypt_token(ID) ->
    encrypt_token(ID, <<>>).

%% @doc 生成绑定设备 DID 的 access token（E2EE-013）。
%% Did=<<>> 表示 legacy 无设备绑定（crypto 写端点将 fail-closed）。
-spec encrypt_token(integer() | binary(), binary()) -> binary().
encrypt_token(ID, Did) ->
    do_encrypt_token(ID, Did, ?TOKEN_VALID, <<"tk">>).

%% @doc Web 刷新仅继承已验证 rtk 的 epoch，防止撤销竞态将旧票升级为新会话。
-spec encrypt_web_token(pos_integer(), binary(), pos_integer(), pos_integer()) -> binary().
encrypt_web_token(Uid, Did, VerifiedEp, DeviceRowId) ->
    encrypt_bound_web_token(Uid, Did, VerifiedEp, DeviceRowId, <<"tk">>).

%% @doc Web Seat 凭证沿用严格设备行绑定，主体仍是 Seat。
-spec encrypt_web_seat_token(pos_integer(), binary(), pos_integer(), pos_integer()) -> binary().
encrypt_web_seat_token(Uid, Did, VerifiedEp, DeviceRowId) ->
    encrypt_bound_web_token(Uid, Did, VerifiedEp, DeviceRowId, <<"seat_tk">>).

encrypt_bound_web_token(Uid, Did, VerifiedEp, DeviceRowId, Sub) ->
    Claims = #{
        <<"uid">> => Uid,
        <<"did">> => Did,
        <<"ep">> => VerifiedEp,
        <<"dsid">> => DeviceRowId,
        <<"sub">> => Sub,
        <<"exp">> => erlang:system_time(second) + ?TOKEN_VALID
    },
    case web_token_claims(Claims, Sub) of
        {ok, _, _, _, _, _, _} -> imboy_jwt:sign(Claims, config_ds:env(jwt_key, <<>>));
        _ -> erlang:error(badarg)
    end.

%% @doc 坐席控制台专用凭证，不签发无设备绑定的 legacy 形态。
-spec encrypt_seat_token(pos_integer(), binary()) -> binary().
encrypt_seat_token(ID, Did) when is_integer(ID), ID > 0, is_binary(Did), Did =/= <<>> ->
    do_encrypt_token(ID, Did, ?TOKEN_VALID, <<"seat_tk">>).

%% @doc 解析token
%% 验证并解析JWT token，提取用户ID、过期时间和主题信息。
%% 验签与 exp/nbf/iat 时间校验由 imboy_jwt（纯 jose）完成；
%% 过期返回 705（可刷新），验签/格式失败返回 706。
%% @param Token JWT token字符串
% token_ds:decrypt_token(token_ds:encrypt_token(1)).
%% @returns 解析结果：成功时返回用户ID、过期时间和主题；失败时返回错误信息
-spec decrypt_token(binary()) ->
    {ok, integer(), integer(), binary(), binary(), integer() | malformed | undefined}
    | {error, integer(), binary() | string(), map()}.
decrypt_token(Token) ->
    decrypt_token(Token, fun token_claims/1).

%% @doc Web Seat 绑定存在时按严格格式解析，旧 Seat 仅保留原生形态。
-spec decrypt_seat_token(binary()) ->
    {ok, integer(), integer(), binary(), binary(), integer() | malformed | undefined}
    | {ok, pos_integer(), integer(), binary(), binary(), pos_integer(), pos_integer()}
    | {error, integer(), binary() | string(), map()}.
decrypt_seat_token(Token) ->
    decrypt_token(Token, fun
        (#{<<"dsid">> := _} = Claims) -> web_token_claims(Claims, <<"seat_tk">>);
        (Claims) -> token_claims(Claims)
    end).

%% @doc 严格 Web 入口消费同一验签/时间校验结果，原 claims 不做 legacy 转换。
-spec decrypt_web_token(binary(), binary()) ->
    {ok, pos_integer(), integer(), binary(), binary(), pos_integer(), pos_integer()}
    | {error, integer(), binary() | string(), map()}.
decrypt_web_token(Token, ExpectedSub) when ExpectedSub =:= <<"tk">>; ExpectedSub =:= <<"rtk">> ->
    decrypt_token(Token, fun(Claims) -> web_token_claims(Claims, ExpectedSub) end);
decrypt_web_token(_, _) ->
    {error, 706, <<"Invalid web token">>, #{}}.

decrypt_token(Token, ParseClaims) ->
    JwtKey = config_ds:env(jwt_key, <<>>),
    try imboy_jwt:verify(Token, JwtKey) of
        {ok, Payload} ->
            ParseClaims(Payload);
        %% exp 严格判定在 imboy_jwt 内完成（默认 leeway 0，与旧链路净语义一致）。
        %% 过期属于"可刷新"，返回 705 避免客户端把 expired 当作 invalid 处理。
        {error, expired} = JWT_ERR ->
            ok = ?DEBUG_LOG(['JWT_EXPIRED', JWT_ERR]),
            {error, 705, "Please refresh token", #{err => JWT_ERR}};
        JWT_ERR ->
            ok = ?DEBUG_LOG(['JWT_ERR', JWT_ERR]),
            {error, 706, "Invalid token", #{err => JWT_ERR}}
    catch
        Class:Reason:Stacktrace ->
            % imboy_jwt:verify 已收敛自身异常；此层兜底的是本函数体内的
            % maps:get / ec_cnv:to_integer / 日志宏等本地异常（如 uid 非法字符串）。
            % 记录 token 解析异常
            ok = ?ERROR_LOG(
                "Token decrypt failed: ~p:~p~nStacktrace: ~p",
                [Class, Reason, Stacktrace]
            ),
            {error, 706, "Invalid token.", #{}}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% Native 保留缺 sub/DID/epoch 的既有兼容与归一化行为。
token_claims(#{<<"dsid">> := _}) ->
    {error, 706, <<"Web token requires Web authentication">>, #{}};
token_claims(Payload) ->
    ID = ec_cnv:to_integer(maps:get(<<"uid">>, Payload, 0)),
    ExpireDAt = maps:get(<<"exp">>, Payload, 0),
    Sub = maps:get(<<"sub">>, Payload, <<"tk">>),
    Did = to_did(maps:get(<<"did">>, Payload, <<>>)),
    Ep = to_epoch_claim(maps:get(<<"ep">>, Payload, undefined)),
    {ok, ID, ExpireDAt, Sub, Did, Ep}.

web_token_claims(
    #{
        <<"uid">> := Uid,
        <<"exp">> := ExpiresAt,
        <<"sub">> := ExpectedSub,
        <<"did">> := Did,
        <<"ep">> := Ep,
        <<"dsid">> := DeviceRowId
    },
    ExpectedSub
) when
    is_integer(Uid),
    Uid > 0,
    is_integer(ExpiresAt),
    is_binary(Did),
    byte_size(Did) > 0,
    is_integer(Ep),
    Ep >= 1,
    is_integer(DeviceRowId),
    DeviceRowId > 0
->
    case unicode:characters_to_list(Did) of
        Chars when is_list(Chars), length(Chars) =< 128 ->
            {ok, Uid, ExpiresAt, ExpectedSub, Did, Ep, DeviceRowId};
        _ ->
            {error, 706, <<"Invalid web token">>, #{}}
    end;
web_token_claims(_, _) ->
    {error, 706, <<"Invalid web token">>, #{}}.

%% @doc 内部token生成函数
%% 根据用户ID、有效时间和主题类型生成JWT token。
%% 用户ID会通过TSID放入payload中。
%% @param ID 用户ID或标识符
%% @param Second token有效期（秒）
%% @param Sub token主题类型（tk表示access token，rtk表示refresh token）
%% @returns 编码后的JWT token
%% @doc 内部签发：uid + 绑定设备 DID（E2EE-013）。
%% Did=<<>> 时不写 did claim（保持与旧 token payload 一致，减少体积）。
%% Task 10 / LT-04：did 绑定 token 同时携带 ep（会话 epoch，签发时现势值；
%% 读取失败回落 1——verify 侧 fail-closed，回落值只会造成"事后被拒"，
%% 不会造成"该拒而放行"）。空 did（legacy 形态）不写 ep，沿 did 豁免先例。
-spec do_encrypt_token(integer() | binary(), binary(), integer(), binary()) -> binary().
do_encrypt_token(ID, Did, Second, Sub) ->
    ExpireDAt = erlang:system_time(second) + Second,
    Base =
        #{
            % sub (subject)：主题
            <<"sub">> => Sub,
            % exp (expiration time)：过期时间
            <<"exp">> => ExpireDAt,
            <<"uid">> => ID
        },
    Data =
        case to_did(Did) of
            <<>> -> Base;
            D -> Base#{<<"did">> => D, <<"ep">> => epoch_at_issue(ID)}
        end,
    JwtKey = config_ds:env(jwt_key, <<>>),
    imboy_jwt:sign(Data, JwtKey).

%% @doc 签发时的会话 epoch 现势值；不可确认时回落 1（安全方向见 do_encrypt_token）。
-spec epoch_at_issue(integer() | binary()) -> non_neg_integer().
epoch_at_issue(ID) ->
    case auth_session_ds:current_epoch(ec_cnv:to_integer(ID)) of
        {ok, E} when is_integer(E), E >= 1 -> E;
        _ -> 1
    end.

%% @doc 归一化 did（容错 string / undefined / 非 binary）。
-spec to_did(term()) -> binary().
to_did(D) when is_binary(D) -> D;
to_did(D) when is_list(D) -> list_to_binary(D);
to_did(_) -> <<>>.

%% @doc 归一化 ep claim：缺省 undefined（legacy 豁免）；整数透传；
%% 其余形态归为 malformed 原子（消费侧 auth_session_ds:revoked/2 fail-closed）。
-spec to_epoch_claim(term()) -> non_neg_integer() | malformed | undefined.
to_epoch_claim(E) when is_integer(E), E >= 1 -> E;
to_epoch_claim(undefined) -> undefined;
to_epoch_claim(_) -> malformed.

%% Caller has locked and read the current epoch and persisted the active device.
issue_device_session(Uid, Did, Epoch) when
    is_integer(Uid),
    Uid > 0,
    is_binary(Did),
    byte_size(Did) > 0,
    is_integer(Epoch),
    Epoch > 0
->
    Key = config_ds:env(jwt_key, <<>>),
    Claims = #{<<"uid">> => Uid, <<"did">> => Did, <<"ep">> => Epoch},
    Now = erlang:system_time(second),
    #{
        <<"token">> => imboy_jwt:sign(
            Claims#{<<"sub">> => <<"tk">>, <<"exp">> => Now + ?TOKEN_VALID}, Key
        ),
        <<"refreshtoken">> => imboy_jwt:sign(
            Claims#{<<"sub">> => <<"rtk">>, <<"exp">> => Now + ?REFRESHTOKEN_VALID}, Key
        )
    }.

%% Preserve the exact device row returned by registration, never a replacement row.
-spec issue_web_session(pos_integer(), binary(), pos_integer(), pos_integer()) -> map().
issue_web_session(Uid, Did, Epoch, DeviceRowId) ->
    Access = encrypt_web_token(Uid, Did, Epoch, DeviceRowId),
    Claims = #{
        <<"uid">> => Uid,
        <<"did">> => Did,
        <<"ep">> => Epoch,
        <<"dsid">> => DeviceRowId,
        <<"sub">> => <<"rtk">>,
        <<"exp">> => erlang:system_time(second) + ?REFRESHTOKEN_VALID
    },
    #{
        <<"token">> => Access,
        <<"refreshtoken">> => imboy_jwt:sign(Claims, config_ds:env(jwt_key, <<>>))
    }.
