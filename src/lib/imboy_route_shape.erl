%%% @doc HTTP 路径形状判定（BE-W01）：cs widget 动态 frame 端点的单一路径真源。
%%%
%%% 路径段形状有两条（CSD-BE-01 起新旧并存，兼容窗口）：
%%%   * 旧：`[api, v1, cs, widget, frame, :installation_id]`（Base 原样保留）；
%%%   * 新：`[w, :public_widget_id]`（hosted-widget-contract S2/S4 的
%%%     `/w/:public_widget_id`，snippet 零 org 申报的 iframe src 落点）。
%%%
%%% 有三处消费：
%%%   * cors_middleware —— frame 路径归属 widget CORS 面（该面豁免 XFO）；
%%%   * security_headers_middleware —— frame 路径豁免 X-Frame-Options；
%%%   * cs_http —— is_credential_surface_path/1 免签直通面登记。
%%% 曾在上述位置各自硬编码（评审指出：改形状时多处必须同步，且无编译期
%%% 约束），收敛到本模块后形状只此一处定义。
%%%
%%% 纯函数、零依赖：api 中间件与 features 模块都可安全下探 lib（反向
%%% features→api / api→features 均不可，故落在 lib）。
-module(imboy_route_shape).

-export([is_cs_widget_frame_path/1]).

%% @doc frame HTML 路径段形状（两条，见模块 doc）。绑定变量段（installation_id /
%% public_widget_id）取任意值，其余段字面精确匹配；相似路径（`/frame`、
%% `/frames`、`/w/a/b` 多一段少一段）一律 false，不放宽。
-spec is_cs_widget_frame_path(binary()) -> boolean().
is_cs_widget_frame_path(Path) when is_binary(Path) ->
    case segments(Path) of
        [<<"api">>, <<"v1">>, <<"cs">>, <<"widget">>, <<"frame">>, _Id] -> true;
        %% CSD-BE-01：/w/:public_widget_id（零凭证导航面；根段 w 全站唯一——
        %% 网站白名单与 /api/*、/adm/*、/static/* 均不占用该段）。
        [<<"w">>, _PublicWidgetId] -> true;
        _Other -> false
    end;
is_cs_widget_frame_path(_Path) ->
    false.

segments(Path) ->
    [S || S <- binary:split(Path, <<"/">>, [global]), S =/= <<>>].
