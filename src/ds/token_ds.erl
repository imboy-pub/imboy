-module(token_ds).

%%%
% token_ds 是 token domain service 缩写
%%%

-export([encrypt_token/1, encrypt_token/2]).
-export([encrypt_refreshtoken/1, encrypt_refreshtoken/2]).
-export([decrypt_token/1]).

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

%% @doc 解析token
%% 验证并解析JWT token，提取用户ID、过期时间和主题信息。
%% 支持5分钟的时钟偏差容错，验证token签名和有效性。
%% @param Token JWT token字符串
% token_ds:decrypt_token(token_ds:encrypt_token(1)).
%% @returns 解析结果：成功时返回用户ID、过期时间和主题；失败时返回错误信息
-spec decrypt_token(binary()) ->
    {ok, integer(), integer(), binary(), binary()}
    | {error, integer(), binary() | string(), map()}.
decrypt_token(Token) ->
    % 容忍 5 分钟时钟偏差
    Opts = #{exp_leeway => 300},
    JwtKey = config_ds:env(jwt_key, <<>>),
    try jwerl:verify(Token, hs256, JwtKey, #{}, Opts) of
        {ok, Payload} ->
            Uid = maps:get(uid, Payload, 0),
            ID = ec_cnv:to_integer(Uid),
            ExpireDAt = maps:get(exp, Payload, <<>>),
            Sub = maps:get(sub, Payload, <<"tk">>),
            % E2EE-013：绑定的设备 DID；legacy token 无此 claim → <<>>。
            Did = to_did(maps:get(did, Payload, <<>>)),
            Now = elib_dt:utc(second),
            if
                ExpireDAt > Now ->
                    {ok, ID, ExpireDAt, Sub, Did};
                true ->
                    {error, 705, "Please refresh token", #{uid => ID, expired_at => ExpireDAt}}
            end;
        %% jwerl 在启用 exp_leeway 后仍会自验 exp，过期时返回 {error, [exp]}。
        %% 语义上属于"可刷新的过期 token"，应返回 705 与上面手动分支一致，
        %% 避免客户端把 expired 当作 invalid 处理。
        {error, ClaimErrs} = JWT_ERR when is_list(ClaimErrs) ->
            case lists:member(exp, ClaimErrs) of
                true ->
                    ok = ?DEBUG_LOG(['JWT_EXPIRED', JWT_ERR]),
                    {error, 705, "Please refresh token", #{err => JWT_ERR}};
                false ->
                    ok = ?DEBUG_LOG(['JWT_ERR', JWT_ERR]),
                    {error, 706, "Invalid token", #{err => JWT_ERR}}
            end;
        JWT_ERR ->
            ok = ?DEBUG_LOG(['JWT_ERR', JWT_ERR]),
            {error, 706, "Invalid token", #{err => JWT_ERR}}
    catch
        Class:Reason:Stacktrace ->
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

%% @doc 内部token生成函数
%% 根据用户ID、有效时间和主题类型生成JWT token。
%% 用户ID会通过TSID放入payload中。
%% @param ID 用户ID或标识符
%% @param Second token有效期（秒）
%% @param Sub token主题类型（tk表示access token，rtk表示refresh token）
%% @returns 编码后的JWT token
%% @doc 内部签发：uid + 绑定设备 DID（E2EE-013）。
%% Did=<<>> 时不写 did claim（保持与旧 token payload 一致，减少体积）。
-spec do_encrypt_token(integer() | binary(), binary(), integer(), binary()) -> binary().
do_encrypt_token(ID, Did, Second, Sub) ->
    ExpireDAt = erlang:system_time(second) + Second,
    Base =
        #{
            % sub (subject)：主题
            sub => Sub,
            % exp (expiration time)：过期时间
            exp => ExpireDAt,
            uid => ID
        },
    Data =
        case to_did(Did) of
            <<>> -> Base;
            D -> Base#{did => D}
        end,
    JwtKey = config_ds:env(jwt_key, <<>>),
    jwerl:sign(Data, hs256, JwtKey).

%% @doc 归一化 did（容错 string / undefined / 非 binary）。
-spec to_did(term()) -> binary().
to_did(D) when is_binary(D) -> D;
to_did(D) when is_list(D) -> list_to_binary(D);
to_did(_) -> <<>>.
