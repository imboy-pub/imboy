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
-include("common.hrl").

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

%% @doc 根据 ID 删除 DDL 记录（仅删除 status=0 的记录）
%% 使用安全的参数化查询，避免SQL注入
-spec delete(integer() | binary()) -> {ok, integer()} | {error, term()}.
delete(Id) ->
    Tb = app_ddl_repo:tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE status = 0 AND id = $1">>,
    imboy_pg:execute(Sql, [ec_cnv:to_integer(Id)]).

get_ddl(WhereMap, OrderBy, Column) ->
    Tb = app_ddl_repo:tablename(),
    % -- 类型 1 升、降级  3 全量安装
    % Where = <<"status=1 AND type = 1 AND new_vsn<=", NewVsn2/binary>>,
    {ok, #{list := Page}} = imboy_pg:page_with_total(Tb, Column, WhereMap, OrderBy, 1, 500),
    Items = [ddl_to_list(get_ddl_field(Item))  || Item <- Page],
    lists:flatten(Items).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

get_ddl_field(Item) when is_map(Item) ->
    maps:get(<<"ddl">>, Item, undefined).

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
