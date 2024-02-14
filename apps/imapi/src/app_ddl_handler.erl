-module(app_ddl_handler).
%%%
% app_version 控制器模块
% app_version controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % 查询
    % PRAGMA user_version
    % 修改
    % PRAGMA user_version = 6
    % ?LOG(State0),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    % Method = cowboy_req:method(Req0),
    Req1 = case Action of
        get_ddl ->
            #{type := Type} = cowboy_req:match_qs([{type, [], <<>>}], Req0),
            get_ddl(Type, Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

get_ddl(<<"upgrade">>, Req0, _State) ->
    % Cos = cowboy_req:header(<<"cos">>, Req0),
    % imboy_log:info(Cos),
    {ok, NewVsn} = imboy_req:get_int(new_vsn, Req0, 0),
    % -- 类型 1 升、降级  3 全量安装
    Where = [
        ["status", "=", 1]
        , ["new_vsn", "<=", NewVsn]
    ],
    OrderBy = <<"new_vsn asc">>,
    Column = <<"ddl">>,
    Ddl = app_ddl_ds:get_ddl(Where, OrderBy, Column),
    imboy_response:success(Req0, #{
        ddl => Ddl
    });

get_ddl(<<"downgrade">>, Req0, _State) ->
    % onDowngrade] is called only when [version] is lower than the last database version.
    %
    {ok, OldVsn} = imboy_req:get_int(old_vsn, Req0, 0),
    {ok, NewVsn} = imboy_req:get_int(new_vsn, Req0, 0),
    {MinVsn, MaxVsn} = if
        OldVsn > NewVsn ->
            {NewVsn, OldVsn};
        true ->
            {OldVsn, NewVsn}
    end,
    % -- 类型 1 升、降级  3 全量安装
    Where = [
        ["status", "=", 1]
        , ["new_vsn", ">=", MinVsn]
        , ["new_vsn", "<=", MaxVsn]
    ],
    OrderBy = <<"new_vsn desc">>,
    Column = <<"down_ddl as ddl">>,
    Ddl = app_ddl_ds:get_ddl(Where, OrderBy, Column),
    imboy_response:success(Req0, #{
        ddl => Ddl
    });

get_ddl(_, Req0, _State) ->
    imboy_response:success(Req0, #{<<"ddl">> => []}).

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
