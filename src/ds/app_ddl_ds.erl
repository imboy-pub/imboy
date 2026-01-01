-module(app_ddl_ds).
%%%
% feedback 业务逻辑模块
% feedback business logic module
%%%

-export ([save/6]).
-export ([get_ddl/3]).
-export ([delete/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% save方法
-spec save(integer(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
save(AdmUserId, NewVsn, OldVsn, Status, Ddl, DownDdl) ->
    % 使用安全的参数化查询，避免SQL注入
    Count = imboy_pg:pluck_value(
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
            imboy_pg:update(
                app_ddl_repo:tablename()
                , Data#{updated_at => imboy_dt:now()}
                , <<"old_vsn = $1 AND new_vsn = $2">>
                , [OldVsn, NewVsn]
            );
        true ->
            app_ddl_repo:add(Data#{created_at => imboy_dt:now()})
    end.

%% @doc 危险函数：直接拼接WHERE子句执行DELETE操作
%% 警告：此函数存在SQL注入风险，仅建议在内部安全场景下使用
%% 建议使用参数化查询的替代方案
-spec delete(binary()) -> ok.
delete(Where) ->
    Tb = app_ddl_repo:tablename(),
    % 警告：此处的SQL拼接存在注入风险，请确保Where参数来源可信
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
        imboy_pg:execute(Sql, []),
    ok.

get_ddl(Where, OrderBy, Column) when is_list(Where) ->
    % 将proplists转换为map格式，并构建WHERE子句
    WhereMap = maps:from_list(Where),
    get_ddl(WhereMap, OrderBy, Column);
get_ddl(WhereMap, OrderBy, Column) ->
    Tb = app_ddl_repo:tablename(),
    % -- 类型 1 升、降级  3 全量安装
    % Where = <<"status=1 AND type = 1 AND new_vsn<=", NewVsn2/binary>>,
    {ok, #{list := Page}} = imboy_pg:page_with_total(Tb, Column, WhereMap, OrderBy, 1, 500),
    Items = [ddl_to_list(proplists:get_value(<<"ddl">>, Item))  || Item <- Page],
    lists:flatten(Items).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

ddl_to_list(undefined) ->
    <<>>;
ddl_to_list(Ddl) ->
    D2 = base64:decode(Ddl),
    D3 = uri_string:unquote(D2),
    D4 = binary:split(D3,<<"\n">>, [global]),
    % 去除注释
    D5 = [R || R <- D4, imboy_str:startswith("--", binary_to_list(R)) == false],
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
