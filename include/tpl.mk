
define tpl_rest_handler
-module($(notdir $(n))).
%%%
% $(subst handler_,,$(notdir $(n))) 控制器模块
% $(subst handler_,,$(notdir $(n))) controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").

init(Req0, State) ->
    % ?LOG(State),
    Req1 = case lists:keyfind(action, 1, State) of
        % {action, demo} ->
            % demo(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.


%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

% demo(Req0, _State) ->
%    {ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
%    dto_resp_json:success(Req0, PostVals, "操作成功.").

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
endef

define tpl_logic
-module($(notdir $(n))).
%%%
% $(subst logic_,,$(notdir $(n))) 业务逻辑模块
% $(subst handler_,,$(notdir $(n))) business logic module
%%%

%-export ([search/1]).

-include("common.hrl").

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
%% ------------------------------------------------------------------
endef

define tpl_repository
-module ($(notdir $(n))).
%%%
% $(subst repo_,,$(notdir $(n))) 相关操作都放到该模块，存储库模块
% $(subst repo_,,$(notdir $(n))) related operations are put in this module, repository module
%%%

-export ([get_by_key/1]).

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

get_by_id(ID) ->
    Sql = <<"SELECT `id` FROM `$(subst repo_,,$(notdir $(n)))` WHERE `id` = ?">>,
    Row = mysql_pool:query(Sql, [ID]),
    % lager:info("~p", Row),
    Row.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
endef

define tpl_transfer
-module ($(notdir $(n))).
%%%
% $(subst transfer_,,$(notdir $(n))) 处理器输出转换模块
% $(subst transfer_,,$(notdir $(n))) handler output conversion module
%%%

-export ([data/2]).

data(User, Friends) ->
    [
        {<<"mine">>, hashids_tlt:replace_id(User)}
        , {<<"friend">>, [hashids_tlt:replace_id(F) || F <-Friends]}
        % {<<"mine">>, User}
        % , {<<"friend">>, Friends}
    ].

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
endef
