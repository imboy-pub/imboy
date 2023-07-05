-module(config_ds).
%%%
% config 领域服务模块
% config domain service 缩写
%%%

-export([env/1, env/2, env/3]).
-export([reload/0, local_reload/0]).

-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

% config_ds:env(test).
% config_ds:env(lager, colors, undefined).
env(Attr) ->
    env(Attr, undefined).
env(Attr, Def) ->
    env(imboy, Attr, Def).
env(App, Attr, Def) ->
    case application:get_env(App, Attr) of
        {ok, Value} ->
            Value;
        _ ->
            Def
    end.

% config_ds:reload().
reload() ->
    Path = config_file(),
    reload(Path).

%% 重新加载 sys.config 配置
%% [config_ds:env(test), config_ds:local_reload(), config_ds:env(test)].
local_reload() ->
    IMBoyEnv = os:getenv("IMBOYENV"),
    From = code:root_dir() ++ "/../../config/sys." ++ IMBoyEnv ++ ".config",
    To = config_file(),
    % Res1 = file:delete(To),
    % lager:error("~p~n", [Res1]),
    file:copy(From, To, infinity),
    % Res2 = file:copy(From, To, infinity),
    % lager:error("copy file res: ~p~n", [Res2]),
    reload(To),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

reload(Path) ->
    {ok, Items} = file:consult(Path),
    % lager:error("~p~n", [Items]),
    [application:set_env(Conf) || Conf <- Items],
    ok.

% config_ds:config_file().
config_file() ->
    {imboy, _, Vsn} = lists:keyfind(imboy, 1, application:which_applications()),
    code:root_dir() ++ "/releases/" ++ Vsn ++ "/sys.config".
