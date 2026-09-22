-module(auth_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

-include("log.hrl").
-include("error_code.hrl").

%% @doc Cowboy中间件执行函数
%% 处理请求的认证和授权验证
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return 中间件执行结果
%% @end
-spec execute(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    Path = auth_ds:remove_last_forward_slash(cowboy_req:path(Req)),
    case Path of
        <<"/static/", _Tail/binary>> ->
            {ok, Req, Env};
        <<"/static/admin/", _Tail/binary>> ->
            % Admin 静态资源直接放行
            {ok, Req, Env};
        <<"/adm/", _Tail/binary>> ->
            % Admin 路由委托给 adm_auth_middleware
            adm_auth_middleware:execute(Req, Env);
        <<"/api/adm/", _Tail/binary>> ->
            % /api 前缀的 Admin 路由同样委托给 adm_auth_middleware，
            % 避免落入客户端默认分支误走 verify_sign 客户端签名门（902）
            adm_auth_middleware:execute(Req, Env);
        <<"/api/internal/v1/", _Tail/binary>> ->
            %% 企业 internal 面（EPGZ-08 W4 接线）：委托
            %% enterprise_internal_middleware 走 Application Credential 认证链
            %% （credential 格式/digest → active/expiry → application active →
            %% organization active → scope → rate fail-closed）。
            %% 不得进 open()/option()（否则匿名可达），也不得落入兜底分支误走
            %% verify_sign 客户端签名门——internal 面无设备/JWT/签名，落兜底
            %% 会被 902 拦死（该分支必须排在 /api/v1/ 之前，因为 internal 前缀
            %% 同时以 /api/ 开头但没有 /api/v1/ 段）。
            enterprise_internal_middleware:execute(Req, Env);
        <<"/api/v1/", _Tail/binary>> ->
            % API v1 路由委托给 auth_middleware_api_v1
            % 2026-07-08 路由由 /v1/* 改名 /api/v1/*，此处前缀当时漏改，
            % 导致 auth_middleware_api_v1 永不被调用（318 条路由全落兜底）：
            % 支付回调/频道 webhook 被 902 拦死、passport 等丢失签名门。
            auth_middleware_api_v1:execute(Req, Env);
        <<"/webrtc/", _Tail/binary>> ->
            {ok, Req, Env};
        %% CSD-BE-01（hosted-widget-contract S4）：`/w/:public_widget_id` 动态
        %% frame HTML——零凭证导航面（iframe src 落点，无 IMBoy 设备/JWT/签名），
        %% 与 /webrtc 同款直通；handler 侧 fail-closed（方法门 + 查询串凭证键
        %% 400 + public_widget_id 形状门），租户归属由 public_widget_id 全局
        %% 反查派生。`/w` 根段全站唯一（CS widget 专属命名空间）。
        <<"/w/", _Tail/binary>> ->
            {ok, Req, Env};
        _ ->
            OpenLi = imboy_router:open(),
            OptionLi = imboy_router:option(),
            InOpenLi = lists:member(Path, OpenLi),
            InOptionLi = lists:member(Path, OptionLi),
            Switch =
                ec_cnv:to_binary(
                    config_ds:env(api_auth_switch, <<"on">>)
                ),
            Res1 =
                if
                    InOpenLi == false, Switch == <<"on">> ->
                        auth_ds:verify_sign(Req, Env);
                    true ->
                        {ok, Req, Env}
                end,
            case Res1 of
                {ok, Req, Env} ->
                    Authorization = cowboy_req:header(<<"authorization">>, Req),
                    auth_ds:condition(InOptionLi, InOpenLi, Authorization, Req, Env);
                Res2 ->
                    Res2
            end
    end.
