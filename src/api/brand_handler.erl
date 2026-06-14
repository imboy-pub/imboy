-module(brand_handler).

%%%===================================================================
%%% @doc 品牌配置 API（运行时白标基础设施）
%%%
%%% 公开端点（无需认证），客户端启动时拉取品牌配置，实现 logo/产品名/
%%% 主题色等配置化白标 —— 私有化客户无需改前端源码重新构建，只需在管理端
%%% 设置 brand_* 配置项即可换品牌。
%%%
%%% 品牌字段统一存 config_ds（brand_* 键），管理端通过现有 config 管理写入：
%%%   config_ds:set(<<"brand_site_name">>, <<"某企业IM">>).
%%% @end
%%%===================================================================
-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, info),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case {Action, Method} of
            {info, <<"GET">>} ->
                info(Req0);
            _ ->
                Req0
        end,
    {ok, Req1, State}.

%% @doc 返回品牌配置（带默认值，缺省即开源 imboy 品牌）
-spec info(cowboy_req:req()) -> cowboy_req:req().
info(Req0) ->
    Brand = #{
        <<"site_name">> => config_ds:get(<<"brand_site_name">>, <<"imboy"/utf8>>),
        <<"logo_url">> => config_ds:get(<<"brand_logo_url">>, <<>>),
        <<"primary_color">> => config_ds:get(<<"brand_primary_color">>, <<"#07C160"/utf8>>),
        <<"accent_color">> => config_ds:get(<<"brand_accent_color">>, <<>>),
        <<"theme">> => config_ds:get(<<"brand_theme">>, <<"light"/utf8>>),
        <<"slogan">> => config_ds:get(<<"brand_slogan">>, <<>>),
        <<"copyright">> => config_ds:get(<<"brand_copyright">>, <<>>),
        <<"company">> => config_ds:get(<<"brand_company">>, <<>>),
        <<"edition">> => imboy_license:edition()
    },
    elib_response:success(Req0, Brand).
