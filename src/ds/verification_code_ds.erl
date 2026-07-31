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
                #{<<"code">> := Code, <<"validity_at">> := ValidityAt} when Now < ValidityAt ->
                    {ok, <<"验证码有效"/utf8>>};
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
