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


% login_success_transfer 按顺序匹配了
% 密码校验取第4位为密码数据 lists:nth(5, Row)
-define (LOGIN_COLUMN, <<"id,account,mobile,password,nickname,avatar,role_id,email">>).

%% ===================================================================
%% API
%% ===================================================================


%%% 运营管理员登录

-spec do_login(binary(), binary()) -> {ok, any()} | {error, any()}.
do_login(_Email, <<>>) ->
    {error, "密码有误"};
do_login(Mobile, Pwd) ->
    Res =
        case imboy_func:is_mobile(Mobile) of
            true ->
                adm_user_repo:find_by_mobile(Mobile, ?LOGIN_COLUMN);
            false ->
                adm_user_repo:find_by_account(Mobile, ?LOGIN_COLUMN)
        end,
    % ?LOG(Res),
    {Check, User} =
        case Res of
            {ok, _, [Row]} when is_tuple(Row) ->
                % ?LOG(['Pwd', Pwd]),
                % 第四个元素为password
                case imboy_password:verify(Pwd, element(4, Row)) of
                    {ok, _} ->
                        {true, Row};
                    {error, Msg} ->
                        {false, Msg}
                end;
            _ ->
                % io:format("res is ~p~n", [Res]),
                {false, []}
        end,
    % ?LOG(['Check', Check, "user" , User]),
    login_success_transfer(Check, User).



%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

% <<"id,account,mobile,password,nickname,avatar,role_id,email">>
-spec login_success_transfer(boolean(), tuple()) -> {ok, map()} | {error, any()}.
login_success_transfer(true, {Id, Account, Mobile, _, Nickname, Avatar, RoleId, Email}) ->
    {ok, #{
           <<"id">> => imboy_hashids:encode(Id),
           <<"mobile">> => Mobile,
           <<"email">> => Email,
           <<"nickname">> => Nickname,
           <<"avatar">> => Avatar,
           <<"account">> => Account,
           <<"role_id">> => RoleId
          }};
% login_success_transfer(_, User) ->
%     ?LOG([User]),
%     {error, "账号或密码错误"}.
login_success_transfer(_, _) ->
    {error, "账号或密码错误"}.

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
