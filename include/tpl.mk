define tpl_imboy.rest_handler
-module($(notdir $(n))).
%%%
% $(subst _handler,,$(notdir $(n))) 控制器模块
% $(subst _handler,,$(notdir $(n))) controller module
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
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        demo_action ->
            demo_action(Method, Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

demo_action(<<"GET">>, Req0, _State) ->
    Body = <<>>,
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).

demo_action(<<"POST">>, Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    $(subst _handler,,$(notdir $(n)))_logic:demo(CurrentUid, Val1, Val2),
    imboy_response:success(Req0, PostVals, "success.").

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
endef

define tpl_imboy.logic
-module($(notdir $(n))).
%%%
% $(subst _logic,,$(notdir $(n))) 业务逻辑模块
% $(subst _logic,,$(notdir $(n))) business logic module
%%%

-export ([demo/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% demo方法描述
-spec demo(Uid::integer(), Val1::binary(), Val2::binary()) -> ok.
demo(Uid, Val1, Val2) ->
    $(subst _logic,,$(notdir $(n)))_repo:demo(Uid, Val1, Val2),
    ok.

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
endef

define tpl_imboy.repository
-module ($(notdir $(n))).
%%%
% $(subst _repo,,$(notdir $(n))) 相关操作都放到该模块，存储库模块
% $(subst _repo,,$(notdir $(n))) related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([demo/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"$(subst _repo,,$(notdir $(n)))">>).

%%% demo方法描述
-spec demo(integer(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
demo(Uid, _Val1, _Val2) ->
    Sql = <<"SELECT id FROM $(subst _repo,,$(notdir $(n))) WHERE id = $$1">>,
    imboy_db:query(Sql, [Uid]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
endef


define tpl_imboy.ds
-module($(notdir $(n))).
%%%
% $(subst _ds,,$(notdir $(n))) 领域服务模块
% $(subst _ds,,$(notdir $(n))) domain service 缩写
%%%

-export ([demo/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% demo方法描述
-spec demo(Uid::integer(), Val1::binary(), Val2::binary()) -> ok.
demo(Uid, Val1, Val2) ->
    $(subst _logic,,$(notdir $(n)))_repo:demo(Uid, Val1, Val2),
    ok.

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
endef
