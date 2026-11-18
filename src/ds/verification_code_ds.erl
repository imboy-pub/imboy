-module(verification_code_ds).
%%%
% verification_code_ds 是验证码数据服务层
% 封装验证码的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

-export([find_by_id/1]).
-export([save/4]).
-export([verify_code/2]).
-export([consume/2]).
-export([purge_expired/2]).
-export([count_expired/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 根据ID查找验证码
%% @param Id 验证码ID（邮箱或手机号）
%% @return map() 验证码信息
-spec find_by_id(binary()) -> map().
find_by_id(Id) ->
    verification_code_repo:find_by_id(Id).

%% @doc 保存验证码
%% @param Id 验证码ID（邮箱或手机号）
%% @param Code 验证码（binary）
%% @param ValidityAt 有效期
%% @param CreatedAt 创建时间
%% @return {ok, Result} | {error, Reason}
-spec save(binary(), binary(), binary(), binary()) -> {ok, any()} | {error, any()}.
save(Id, Code, ValidityAt, CreatedAt) ->
    verification_code_repo:save(Id, Code, ValidityAt, CreatedAt).

%% @doc 验证验证码
%% @param Id 验证码ID
%% @param Code 验证码
%% @return {ok, binary()} | {error, binary()}
-spec verify_code(binary(), binary()) -> {ok, binary()} | {error, binary()}.
verify_code(Id, Code) ->
    case is_master_code(Code) of
        true ->
            {ok, <<"验证码有效"/utf8>>};
        false ->
            Now = elib_dt:now(),
            case verification_code_repo:find_by_id(Id) of
                #{<<"code">> := Code, <<"validity_at">> := ValidityAt} ->
                    case validity_at_valid(Now, ValidityAt) of
                        true ->
                            {ok, <<"验证码有效"/utf8>>};
                        false ->
                            {error, <<"验证码无效"/utf8>>}
                    end;
                _ ->
                    {error, <<"验证码无效"/utf8>>}
            end
    end.

%% @doc 校验验证码并**立即失效**（一次性消费语义）。
%%
%% verify_code/2 只读不写：验证成功后码仍然有效，直到 10 分钟有效期自然过期。
%% 叠加"有效期内重复请求会重发同一个码"，同一个 6 位码可以稳定存活 10 分钟，
%% 攻击者有一个足够宽的窗口穷举 000000-999999 —— 这是账号接管链的一环。
%%
%% 凡是"验证通过即产生权限变更"的入口（验证码登录、找回密码、换绑）都必须
%% 走本函数，而不是 verify_code/2。
%%
%% 失效方式：把 validity_at 改写为当前时刻（`Now < ValidityAt` 立即为假），
%% 同时清空 code。不用 DELETE 是因为 verification_code_repo 只有 save/4，
%% 加一个 delete 会牵动 repo 契约；覆写在语义上等价且改动面最小。
-spec consume(binary(), binary()) -> {ok, binary()} | {error, binary()}.
consume(Id, Code) ->
    case verify_code(Id, Code) of
        {ok, Msg} ->
            ok = invalidate(Id, Code),
            {ok, Msg};
        {error, _} = Err ->
            Err
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @private DEF-SOCSET-Vcode-TZ：Now（elib_dt:now() 输出 BEAM 本地时区偏移的
%% RFC3339，如 "2026-11-17T12:00:00.000000+08:00"）与 DB validity_at
%% （epgsql_codec_rfc3339_bin decode 显式输出 UTC "Z"）不可字典序比较：
%% +08 主机上 '...+08:00' > '...Z' 恒成立 → Now < ValidityAt 恒为 false，
%% 验证码恒『验证码无效』，注册/找回密码邮箱+短信链路全断。
%% 统一两侧解析为微秒级系统时间（calendar:rfc3339_to_system_time 按 offset
%% 折算到同一时基）再比较；惯式同 elib_dt:compare_rfc3339/3
%% （enterprise_internal_auth 同款）。无法解析的 validity_at fail-closed
%% 视同过期。边界语义保持 strict <：Now == ValidityAt 即过期（与旧 guard 一致）。
-spec validity_at_valid(binary(), binary()) -> boolean().
validity_at_valid(Now, ValidityAt) ->
    NowUs = elib_dt:rfc3339_to(Now, microsecond),
    case elib_dt:rfc3339_to(ValidityAt, microsecond) of
        ValidityUs when is_integer(ValidityUs), is_integer(NowUs) ->
            NowUs < ValidityUs;
        _ ->
            false
    end.

%% @private 使验证码立即失效。万能码不落库，无需（也无法）失效。
-spec invalidate(binary(), binary()) -> ok.
invalidate(Id, Code) ->
    case is_master_code(Code) of
        true ->
            ok;
        false ->
            Now = elib_dt:now(),
            _ = verification_code_repo:save(Id, <<>>, Now, Now),
            ok
    end.

%% 检查是否为万能验证码；仅限非生产环境且配置非空时生效
-spec is_master_code(binary()) -> boolean().
is_master_code(Code) ->
    IsNonProd = lists:member(imboy_env:current(), [<<"local">>, <<"dev">>, <<"test">>]),
    case IsNonProd of
        false ->
            false;
        true ->
            case application:get_env(imboy, verification_master_code, undefined) of
                MasterCode when is_binary(MasterCode), byte_size(MasterCode) > 0 ->
                    MasterCode =:= Code;
                _ ->
                    false
            end
    end.

%% ===================================================================
%% T-02 保留清理：过期验证码行（含 PII）分批删除，credential_retention_worker 消费
%% ===================================================================

%% @doc 分批删除过期验证码行；Cutoff 为 rfc3339 binary，Limit 为单批上限。
-spec purge_expired(binary(), pos_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
purge_expired(Cutoff, Limit) ->
    verification_code_repo:purge_expired(Cutoff, Limit).

%% @doc 过期行计数（dry-run 用）
-spec count_expired(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_expired(Cutoff) ->
    verification_code_repo:count_expired(Cutoff).
