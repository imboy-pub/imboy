-module(imboy_dtl).
-eqwalizer(enable).

%%% @doc 模板引擎辅助模块
%%% 提供模板渲染和参数构建功能

% -export([template/2]).
-export([imadm_param/1]).
-export([template/3]).

%% @doc 渲染模板
%% @param Name 模板名称（atom）
%% @param Vars 模板变量列表
%% @param AppName 应用名称（atom）
%% @returns {ok, Rendered} 渲染结果
%% @example
%% imboy_dtl:template(login_dtl, [], imadm).
-spec template(atom(), list(), atom()) -> {ok, list()}.
template(Name, Vars, AppName) ->
    Path = elib_cnv:implode("", [code:priv_dir(AppName), "/template/admin/", Name, ".html"]),
    _ = erlydtl:compile(binary_to_list(Path), Name),
    Name:render(Vars).

%% @doc 构建管理后台模板参数
%% @param State 包含 adm_user_id 的 map
%% @returns 参数列表 [{atom(), binary() | string()}]
-spec imadm_param(map()) -> list({atom(), binary() | string()}).
imadm_param(State) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    Key = {adm_user_sample, AdmUserId},
    U = adm_user_logic:find(AdmUserId, <<"id,nickname">>, Key),
    [
        {system_name, "IMBoy Admin System"},
        {adm_nickname, maps:get(<<"nickname">>, U, <<>>)}
    ].
