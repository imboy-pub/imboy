
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
endef

define tpl_as
-module($(notdir $(n))).
%%%
% $(notdir $(n)) 是 $(notdir $(n:_as=)) application service 缩写
%%%

%-export ([search/1]).

-include("common.hrl").

%%% 查找非好友
%search(Uid) ->
    % 只能够搜索“用户被允许搜索”的用户
    %
 %   ok.

%% Internal.
endef

define tpl_repo
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

%% Internal.
endef

define tpl_ass
-module ($(notdir $(n))).
%%%
% $(notdir $(n)) 是 $(notdir $(n:_repo=)) application assembler 缩写
%%%

-export ([data/2]).

data(User, Friends) ->
    [
        {<<"mine">>, hashids_tlt:replace_id(User)}
        , {<<"friend">>, [hashids_tlt:replace_id(F) || F <-Friends]}
        % {<<"mine">>, User}
        % , {<<"friend">>, Friends}
    ].

%% Internal.
endef
