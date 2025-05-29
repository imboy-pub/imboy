-module(adm_passport_logic).
%%%
% adm_passport 业务逻辑模块
% adm_passport business logic module
%%%


-export([do_login/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").


-define (LOGIN_COLUMN, <<"id,account,mobile,password,nickname,avatar,role_id,email,status">>).

%% ===================================================================
%% API
%% ===================================================================


%%% 运营管理员登录

-spec do_login(binary(), binary()) -> {ok, any()} | {error, any()}.
do_login(_Email, <<>>) ->
    {error, "密码有误"};
do_login(Mobile, Pwd) ->
    User = case imboy_func:is_mobile(Mobile) of
        true ->
            adm_user_repo:find_by_mobile(Mobile, ?LOGIN_COLUMN);
        false ->
            adm_user_repo:find_by_account(Mobile, ?LOGIN_COLUMN)
    end,
    % ?DEBUG_LOG(User),
    verify_user(Pwd, User).



%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-


-spec verify_user(binary(), map()) -> {ok, map()} | {error, any()}.
verify_user(Pwd, User) ->
    Pwd2 = maps:get(<<"password">>, User, <<>>),
    % 状态: -1 删除  0 禁用  1 启用
    Status = maps:get(<<"status">>, User, -2),
    case imboy_password:verify(Pwd, Pwd2) of
        {ok, _} when Status == -2 ->
            {error, "账号不存在"};
        {ok, _} when Status == -1 ->
            {error, "账号不存在或者已删除"};
        {ok, _} when Status == 0 ->
            {error, "账号被禁用"};
        {ok, _} when Status == 1 ->
            Id = maps:get(<<"id">>, User),
            {ok, #{
               <<"id">> => imboy_hashids:encode(Id),
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

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
