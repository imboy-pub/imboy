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
%% 供测试与前端契约：品牌字段默认值与校验回退（纯函数，不触库）
-export([defaults/0, normalize/1, config_key/1]).

-include("log.hrl").

%% 缺省即开源 imboy 品牌。URL 类与联系方式类默认留空——
%% 客服/隐私链接属对外联系方式，必须由部署方人工填写，代码不得预置任何值。
-define(DEFAULT_SITE_NAME, <<"imboy"/utf8>>).
-define(DEFAULT_PRIMARY_COLOR, <<"#07C160">>).
-define(DEFAULT_THEME, <<"light">>).

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

%% @doc 返回品牌配置：逐字段读 config_ds，再经 normalize/1 校验；
%% 任一字段缺失或非法都回退默认值，保证客户端永远拿到可用配置。
-spec info(cowboy_req:req()) -> cowboy_req:req().
info(Req0) ->
    Raw = maps:map(
        fun(Field, Default) -> config_ds:get(config_key(Field), Default) end,
        defaults()
    ),
    Brand = maps:put(<<"edition">>, imboy_license:edition(), normalize(Raw)),
    elib_response:success(Req0, Brand).

%% @doc 品牌字段默认值（缺省 = 开源 imboy 品牌）
-spec defaults() -> map().
defaults() ->
    #{
        <<"site_name">> => ?DEFAULT_SITE_NAME,
        <<"logo_url">> => <<>>,
        <<"splash_url">> => <<>>,
        <<"primary_color">> => ?DEFAULT_PRIMARY_COLOR,
        <<"accent_color">> => <<>>,
        <<"theme">> => ?DEFAULT_THEME,
        <<"slogan">> => <<>>,
        <<"copyright">> => <<>>,
        <<"company">> => <<>>,
        <<"support_url">> => <<>>,
        <<"privacy_url">> => <<>>
    }.

%% @doc config 表中的键名：brand_ 前缀 + 字段名
-spec config_key(binary()) -> binary().
config_key(Field) -> <<"brand_", Field/binary>>.

%% @doc 校验并回退：只保留 defaults/0 中的字段（多余键丢弃），
%% 每个字段非法即回退该字段默认值（不整体回退，避免一个坏字段废掉整套品牌）。
-spec normalize(map()) -> map().
normalize(Raw) when is_map(Raw) ->
    maps:map(
        fun(Field, Default) ->
            Value = maps:get(Field, Raw, Default),
            case is_valid(Field, Value) of
                true -> Value;
                false -> Default
            end
        end,
        defaults()
    );
normalize(_) ->
    defaults().

-spec is_valid(binary(), term()) -> boolean().
is_valid(<<"site_name">>, V) ->
    is_binary(V) andalso V =/= <<>>;
is_valid(<<"theme">>, V) ->
    V =:= <<"light">> orelse V =:= <<"dark">>;
is_valid(<<"primary_color">>, V) ->
    is_hex_color(V);
is_valid(<<"accent_color">>, V) ->
    V =:= <<>> orelse is_hex_color(V);
is_valid(<<"logo_url">>, V) ->
    is_http_url(V);
is_valid(<<"splash_url">>, V) ->
    is_http_url(V);
is_valid(<<"support_url">>, V) ->
    is_http_url(V);
is_valid(<<"privacy_url">>, V) ->
    is_http_url(V);
is_valid(_Field, V) ->
    is_binary(V).

%% 只接受 http(s) 绝对地址或空，挡掉 javascript:/data: 等可注入前端的 scheme
-spec is_http_url(term()) -> boolean().
is_http_url(<<>>) -> true;
is_http_url(<<"https://", Rest/binary>>) -> Rest =/= <<>>;
is_http_url(<<"http://", Rest/binary>>) -> Rest =/= <<>>;
is_http_url(_) -> false.

-spec is_hex_color(term()) -> boolean().
is_hex_color(<<$#, Hex:6/binary>>) ->
    lists:all(fun is_hex_digit/1, binary_to_list(Hex));
is_hex_color(_) ->
    false.

-spec is_hex_digit(byte()) -> boolean().
is_hex_digit(C) when C >= $0, C =< $9 -> true;
is_hex_digit(C) when C >= $a, C =< $f -> true;
is_hex_digit(C) when C >= $A, C =< $F -> true;
is_hex_digit(_) -> false.
