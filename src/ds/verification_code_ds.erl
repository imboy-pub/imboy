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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
