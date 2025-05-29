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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% save方法
-spec save(integer(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
save(AdmUserId, NewVsn, OldVsn, Status, Ddl, DownDdl) ->
    Where = <<"old_vsn=", (ec_cnv:to_binary(OldVsn))/binary, " AND new_vsn= ", (ec_cnv:to_binary(NewVsn))/binary>>,
    Count = imboy_db:pluck(
        <<"app_ddl">>
        , Where
        , <<"count(*)">>
        , 0),
    Data = #{
        ddl => Ddl
        , down_ddl => DownDdl
        , admin_user_id => AdmUserId
        , old_vsn => ec_cnv:to_integer(OldVsn)
        , new_vsn => ec_cnv:to_integer(NewVsn)
        , status => ec_cnv:to_integer(Status)
    },
    ?DEBUG_LOG([count, Count]),
    if Count > 0 ->
            imboy_db:update(
                app_ddl_repo:tablename()
                , Where
                , Data#{updated_at => imboy_dt:now()}
            );
        true ->
            app_ddl_repo:add(Data#{created_at => imboy_dt:now()})
    end.

-spec delete(binary()) -> ok.
delete(Where) ->
    Tb = app_ddl_repo:tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
    ?DEBUG_LOG([Sql]),
    imboy_db:execute(Sql, []),
    ok.

get_ddl(Where, OrderBy, Column) when is_list(Where) ->
    W2 = imboy_db:assemble_where(Where),
    get_ddl(W2, OrderBy, Column);
get_ddl(Where, OrderBy, Column) ->
    Tb = app_ddl_repo:tablename(),
    % -- 类型 1 升、降级  3 全量安装
    % Where = <<"status=1 AND type = 1 AND new_vsn<=", NewVsn2/binary>>,
     Page = imboy_db:page_for_where(Tb, 500, 0, Where, OrderBy, Column),
    % ?DEBUG_LOG(Page),
    % Page.
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
