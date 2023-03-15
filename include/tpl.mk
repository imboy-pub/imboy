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
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        % action_atom ->
            % demo(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% demo(Req0, _State) ->
%    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
%    imboy_response:success(Req0, PostVals, "操作成功.").

%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

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

%-export ([search/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------
%%% 查找非好友
%search(Uid) ->
    % 只能够搜索“用户被允许搜索”的用户
    %
%   ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------------

%

%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

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

-export ([get_by_key/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

get_by_id(ID) ->
    Sql = <<"SELECT `id` FROM `$(subst _repo,,$(notdir $(n)))` WHERE `id` = ?">>,
    Row = mysql_pool:query(Sql, [ID]),
    % lager:info("~p", Row),
    Row.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%

%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
endef
