
define tpl_rest_handler
-module($(notdir $(n))).
-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").

init(Req0, State) ->
    % ?LOG(State),
    Req1 = case lists:keyfind(action, 1, State) of
        % {action, online} ->
            % online(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
endef

define tpl_logic
-module($(notdir $(n))).
%%%
% $(notdir $(n)) 是 $(notdir $(n:_as=)) application logic 缩写
%%%

%-export ([search/1]).

-include("common.hrl").

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
% $(notdir $(n)) 是 $(notdir $(n:_repo=)) repository 缩写
%%%

-export ([get_by_key/1]).

get_by_key(Key) ->
    Sql = <<"SELECT `value` FROM `config` WHERE `key` = ?">>,
    Row = mysql_pool:query(Sql, [Key]),
    % lager:info("~p", Row),
    Row.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
endef

define tpl_transfer
-module ($(notdir $(n))).
%%%
% $(notdir $(n)) 是 $(notdir $(n:_repo=)) application transfer 缩写
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
