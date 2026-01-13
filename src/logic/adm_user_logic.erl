-module(adm_user_logic).
%%%
% adm_user 业务逻辑模块
% adm_user business logic module
%%%

-export ([find/3]).
-export ([list/2, count/0]).
-export ([save/1, update/2, delete/1]).
-export ([assign_roles/2, update_status/2, reset_password/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 查找管理员用户
%% 根据用户ID和指定列查找管理员信息，支持缓存
%% @param Uid 管理员用户ID
%% @param Column 需要查询的列
%% @param Key 缓存键
%% @return map() 管理员用户信息
%%% demo方法描述
-spec find(integer(), binary(), tuple()) -> map().
find(Uid, Column, Key) ->
    Fun = fun() ->
        adm_user_ds:find_by_id(Uid, Column)
    end,
    imboy_cache:memo(Fun, Key, 7200).

%% @doc 获取管理员用户列表（分页）
-spec list(pos_integer(), pos_integer()) -> {ok, [map()]} | {error, term()}.
list(Page, Size) ->
    adm_user_ds:list(Page, Size).

%% @doc 统计管理员用户总数
-spec count() -> {ok, integer()}.
count() ->
    adm_user_ds:count().

%% @doc 保存管理员用户（创建或更新）
-spec save(map()) -> {ok, integer()} | {error, any()}.
save(#{<<"id">> := Id} = Data) when is_integer(Id), Id > 0 ->
    update(Id, Data);
save(Data) ->
    % 检查账号是否已存在
    Account = maps:get(<<"account">>, Data),
    case adm_user_ds:find_by_account(Account, <<"id">>) of
        #{<<"id">> := _} ->
            {error, <<"账号已存在"/utf8>>};
        _ ->
            % 密码加密
            Password = maps:get(<<"password">>, Data),
            PasswordHash = elib_password:generate(elib_hasher:md5(Password)),
            Now = elib_dt:now(),
            SaveData = Data#{
                <<"password">> => PasswordHash,
                <<"login_count">> => 0,
                <<"created_at">> => Now
            },
            adm_user_ds:save(SaveData)
    end.

%% @doc 更新管理员用户
-spec update(integer(), map()) -> {ok, integer()} | {error, any()}.
update(Id, Data) ->
    % 检查账号是否重复
    Account = maps:get(<<"account">>, Data, <<>>),
    case adm_user_ds:find_by_account(Account, <<"id">>) of
        #{<<"id">> := Id} ->
            % 账号相同，可以更新
            UpdateData = maps:without([<<"id">>], Data),
            adm_user_ds:update(Id, UpdateData);
        #{<<"id">> := _} ->
            {error, <<"账号已存在"/utf8>>};
        _ ->
            UpdateData = maps:without([<<"id">>], Data),
            adm_user_ds:update(Id, UpdateData)
    end.

%% @doc 删除管理员用户（软删除）
-spec delete(integer()) -> {ok, integer()} | {error, any()}.
delete(Id) ->
    % 不允许删除超级管理员（role_id 包含 1）
    case adm_user_ds:find_by_id(Id, <<"id,role_id">>) of
        #{<<"role_id">> := RoleIds} when is_list(RoleIds) ->
            case lists:member(1, RoleIds) of
                true -> {error, <<"不能删除超级管理员"/utf8>>};
                false -> adm_user_ds:delete(Id)
            end;
        _ ->
            {error, <<"用户不存在"/utf8>>}
    end.

%% @doc 为管理员分配角色
-spec assign_roles(integer(), [integer()]) -> ok | {error, any()}.
assign_roles(UserId, RoleIds) when is_list(RoleIds) ->
    UpdateData = #{<<"role_id">> => RoleIds},
    case adm_user_ds:update(UserId, UpdateData) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end;
assign_roles(_, _) ->
    {error, <<"角色ID列表格式错误"/utf8>>}.

%% @doc 更新管理员用户状态
-spec update_status(integer(), integer()) -> ok | {error, any()}.
update_status(UserId, Status) when Status >= 0, Status =< 1 ->
    % 不允许禁用超级管理员
    case adm_user_ds:find_by_id(UserId, <<"id,role_id">>) of
        #{<<"role_id">> := RoleIds} when is_list(RoleIds) ->
            case lists:member(1, RoleIds) of
                true when Status =:= 0 -> {error, <<"不能禁用超级管理员"/utf8>>};
                _ ->
                    case adm_user_ds:update(UserId, #{<<"status">> => Status}) of
                        {ok, _} -> ok;
                        {error, Reason} -> {error, Reason}
                    end
            end;
        _ ->
            {error, <<"用户不存在"/utf8>>}
    end;
update_status(_, _) ->
    {error, <<"状态值无效"/utf8>>}.

%% @doc 重置管理员密码
-spec reset_password(integer(), binary()) -> ok | {error, any()}.
reset_password(UserId, NewPassword) when byte_size(NewPassword) > 0 ->
    PasswordHash = elib_password:generate(elib_hasher:md5(NewPassword)),
    case adm_user_ds:update(UserId, #{<<"password">> => PasswordHash}) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end;
reset_password(_, _) ->
    {error, <<"密码不能为空"/utf8>>}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

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
