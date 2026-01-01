-module(adm_app_version_logic).
%%%
% adm_app_version 业务逻辑模块
% adm_app_version business logic module
%%%

-export ([delete/1]).
-export ([save/1]).

-export([vsn_sort/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 保存或更新应用版本信息
%% 根据 ID 存在与否决定是更新还是新建记录
%% @param Data 包含版本信息的数据映射
%% @return any() 数据库操作结果
% adm_app_version_logic:save()
-spec save(map()) -> any().
save(Data) ->
    % ?DEBUG_LOG([count, Count, " Where ", Where]),
    Id = ec_cnv:to_integer(maps:get(id, Data)),
    if Id > 0 ->
            imboy_pg:update(
                app_version_repo:tablename()
                , Data#{updated_at => imboy_dt:now()}
                , <<"id = $1">>
                , [Id]
            );
        true ->
            D2 = maps:remove(id, Data),
            app_version_repo:add(D2#{created_at => imboy_dt:now()})
    end.

%% @doc 删除应用版本记录
%% 根据 WHERE 条件删除对应的版本记录
%% @param Where 删除条件的 SQL WHERE 子句
%% @return ok 操作成功标识
-spec delete(binary()) -> ok.
delete(Where) ->
    Tb = app_version_repo:tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
    % ?DEBUG_LOG([Sql]),
    imboy_pg:execute(Sql, []),
    ok.


% adm_app_version_logic:vsn_sort(<<"0.2">>).
% adm_app_version_logic:vsn_sort(<<"0.2.22">>).
%  adm_app_version_logic:vsn_sort(<<"10.102.22">>).
%% @doc 将版本号转换为数值用于排序
%% 支持语义化版本号格式，转换为单一数值便于数据库排序
%% @param Vsn 版本号字符串，如 "1.2.3"
%% @return integer() 转换后的数值，用于排序比较
-spec vsn_sort(binary()) -> integer().
vsn_sort(Vsn) ->
    {Major2, Minor2, Patch2} = case ec_semver:parse(Vsn) of
        {{Major, Minor, Patch, _}, _} ->
            {Major, Minor, Patch};
        {{Major, Minor, Patch}, _} ->
            {Major, Minor, Patch};
        {{Major, Minor}, _} ->
            {Major, Minor, 0};
        {Major, _} when is_integer(Major) ->
            {Major, 0, 0};
        {_, _} ->
            {0, 0, 0}
    end,
    Major2 * 1_000_000 + Minor2 * 1_000 + Patch2.

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
