-module(adm_passport_logic).
%%%
% adm_passport 业务逻辑模块
% adm_passport business logic module
%%%


-export([do_login/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% @doc 运营管理员登录
%% 验证管理员账号和密码，返回管理员信息
%% @param Mobile 管理员账号（手机号或账号名）
%% @param Pwd 密码（明文）
%% @return {ok, AdminUser} | {error, Reason} 登录结果
%%% 运营管理员登录
-spec do_login(binary(), binary()) -> {ok, map()} | {error, binary()}.
do_login(_Email, <<>>) ->
    {error, <<"密码有误"/utf8>>};
do_login(Mobile, Pwd) ->
    User = case elib_type:is_mobile(Mobile) of
        true ->
            adm_user_ds:find_by_mobile(Mobile, ?LOGIN_COLUMN);
        false ->
            adm_user_ds:find_by_account(Mobile, ?LOGIN_COLUMN)
    end,
    verify_user(Pwd, User).



%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-


-spec verify_user(binary(), map()) -> {ok, map()} | {error, any()}.
verify_user(Pwd, User) ->
    Pwd2 = maps:get(<<"password">>, User, <<>>),
    % 状态: -1 删除  0 禁用  1 启用
    Status = maps:get(<<"status">>, User, -2),
    case elib_password:verify(Pwd, Pwd2) of
        {ok, _} when Status == -2 ->
            {error, <<"账号不存在"/utf8>>};
        {ok, _} when Status == -1 ->
            {error, <<"账号不存在或者已删除"/utf8>>};
        {ok, _} when Status == 0 ->
            {error, <<"账号被禁用"/utf8>>};
        {ok, _} when Status == 1 ->
            Id = maps:get(<<"id">>, User),
            {ok, #{
               <<"id">> => elib_hashids:encode(Id),
               <<"mobile">> => maps:get(<<"mobile">>, User),
               <<"email">> => maps:get(<<"email">>, User),
               <<"nickname">> => maps:get(<<"nickname">>, User),
               <<"avatar">> => maps:get(<<"avatar">>, User),
               <<"account">> => maps:get(<<"account">>, User),
               <<"role_id">> => maps:get(<<"role_id">>, User)
              }};
        {error, Msg} ->
            {error, Msg}
    end.


%% ===================================================================
%% EUnit tests.
%% ===================================================================

