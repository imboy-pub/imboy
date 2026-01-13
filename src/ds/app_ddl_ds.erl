-module(app_ddl_ds).
%%%
% app_ddl 领域服务模块
% app_ddl domain service 缩写
%%%

%% @doc 保存或更新 DDL 记录
%% 根据版本号检查记录是否存在，存在则更新，不存在则新增
%% @param AdmUserId 管理员用户ID
%% @param NewVsn 新版本号
%% @param OldVsn 旧版本号
%% @param Status 状态
%% @param Ddl DDL 语句（base64 编码）
%% @param DownDdl 回滚 DDL 语句（base64 编码）
%% @returns {ok, list(), list()} | {error, any()}
-export ([save/6]).

%% @doc 根据 ID 删除 DDL 记录（仅删除 status=0 的记录）
%% @param Id DDL 记录 ID
%% @returns {ok, integer()} | {error, term()}
-export ([delete/1]).

%% @doc 获取 DDL 脚本列表
%% @param WhereMap 查询条件映射
%% @param OrderBy 排序条件
%% @param Column 要查询的列
%% @returns DDL 语句列表
-export ([get_ddl/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% save方法
-spec save(integer(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
save(AdmUserId, NewVsn, OldVsn, Status, Ddl, DownDdl) ->
    % 使用安全的参数化查询，避免SQL注入
    Count = elib_pg:pluck_value(
        <<"public.app_ddl">>
        , <<"count(*)">>
        , #{old_vsn => OldVsn, new_vsn => NewVsn}
        , #{}, 0),
    Data = #{
        ddl => Ddl
        , down_ddl => DownDdl
        , admin_user_id => AdmUserId
        , old_vsn => ec_cnv:to_integer(OldVsn)
        , new_vsn => ec_cnv:to_integer(NewVsn)
        , status => ec_cnv:to_integer(Status)
    },
        if Count > 0 ->
            % 使用安全的参数化查询，避免SQL注入
            elib_pg:update(
                app_ddl_repo:tablename()
                , Data#{updated_at => elib_dt:now()}
                , <<"old_vsn = $1 AND new_vsn = $2">>
                , [OldVsn, NewVsn]
            );
        true ->
            app_ddl_repo:add(Data#{created_at => elib_dt:now()})
    end.

%% @doc 根据 ID 删除 DDL 记录（仅删除 status=0 的记录）
%% 使用安全的参数化查询，避免SQL注入
-spec delete(integer() | binary()) -> {ok, integer()} | {error, term()}.
delete(Id) ->
    Tb = app_ddl_repo:tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE status = 0 AND id = $1">>,
    elib_pg:execute(Sql, [ec_cnv:to_integer(Id)]).

%% @doc 获取 DDL 脚本列表
%% @param WhereMap 查询条件映射
%% @param OrderBy 排序条件
%% @param Column 要查询的列
%% @returns DDL 语句列表
-spec get_ddl(map(), binary(), binary()) -> [binary()].
get_ddl(WhereMap, OrderBy, Column) ->
    Tb = app_ddl_repo:tablename(),
    % -- 类型 1 升、降级  3 全量安装
    % Where = <<"status=1 AND type = 1 AND new_vsn<=", NewVsn2/binary>>,
    {ok, #{list := Page}} = elib_pg:page_with_total(Tb, Column, WhereMap, OrderBy, 1, 500),
    Items = [ddl_to_list(get_ddl_field(Item))  || Item <- Page],
    lists:flatten(Items).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%% @doc 从 DDL 记录中提取 DDL 字段
%% @param Item DDL 记录映射
%% @returns DDL 字段值或 undefined
-spec get_ddl_field(map()) -> binary() | undefined.
get_ddl_field(Item) when is_map(Item) ->
    maps:get(<<"ddl">>, Item, undefined).

%% @doc 将 base64 编码的 DDL 转换为 SQL 语句列表
%% @param Ddl base64 编码的 DDL 或 undefined
%% @returns SQL 语句列表
-spec ddl_to_list(binary() | undefined) -> [binary()].
ddl_to_list(undefined) ->
    <<>>;
ddl_to_list(Ddl) ->
    D2 = base64:decode(Ddl),
    D3 = uri_string:unquote(D2),
    D4 = binary:split(D3,<<"\n">>, [global]),
    % 去除注释
    D5 = [R || R <- D4, elib_str:startswith("--", binary_to_list(R)) == false],
    D6 = iolist_to_binary(D5),
    binary:split(D6,<<";">>, [global]).

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
