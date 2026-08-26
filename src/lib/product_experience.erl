-module(product_experience).

%%%
% 产品体验开关（服务端唯一真相源，双体验 v2.5.2 §4.1 / Decision Brief R4.4）
%
% 安装级 IMBOY_PRODUCT_EXPERIENCE 决定客户端呈现 chat（经典 IM）还是
% workspace（协作工作台）形态。命名与 product_profile 严格隔离：
%   product_profile（community|enterprise）= 销售/版本档位，非法值 fail-closed 拒启；
%   product_experience（chat|workspace）   = 体验开关，缺失/非法值 fail-safe 为 chat。
% 两者并存、互不覆盖。
%
% 唯一注入点：imboy_env:override_from_env/0 启动时一次性写入
% {imboy, product_experience}，运行期只读；切换 = 修改部署配置 + 受控重启，
% 无运行时写接口（Admin 仅只读展示，T11）。
%
% config_version = hex(sha256("experience=" ++ Effective ++ ";app=" ++ AppVsn))
% 前 16 个 hex 字符：相同部署恒定；effective experience 或应用发布版本任一
% 变化必变化。客户端（AppInitializer）缓存并在下次启动比对失效，
% /api/v1/init 下发 effective_product_experience / config_version 两字段。
%%%

-export([effective/0, effective_binary/0, config_version/0, digest/2]).

-type experience() :: chat | workspace.

%% @doc 当前有效产品体验（原子）。
%% 读 {imboy, product_experience}（imboy_env 启动注入），缺失/未知类型/
%% 非法值一律降级 chat（fail-safe，见模块头）。
-spec effective() -> experience().
effective() ->
    normalize(application:get_env(imboy, product_experience, chat)).

%% @doc 当前有效产品体验（binary，线协议形态），供 handler 直接下发。
-spec effective_binary() -> binary().
effective_binary() ->
    to_binary(effective()).

%% @doc 配置版本摘要：见模块头算法说明。
%% 输入只有"有效 experience"与"应用发布版本"（application:get_key(imboy, vsn)），
%% 相同部署恒定，任一变化必变化。
-spec config_version() -> binary().
config_version() ->
    digest(effective(), app_vsn()).

%% @doc 纯函数摘要（便于测试与外部复算）：experience 归一化后与 vsn 一起
%% 参与 sha256，取前 16 个 hex 字符（小写，`shasum -a 256` 复算一致）。
%% golden 向量见 product_experience_tests:config_version_golden_test/0。
-spec digest(experience() | binary() | string(), binary() | string()) -> binary().
digest(Experience, AppVsn) ->
    Payload = <<
        "experience=",
        (to_binary(normalize(Experience)))/binary,
        ";app=",
        (unicode:characters_to_binary(AppVsn))/binary
    >>,
    Hex = binary:encode_hex(crypto:hash(sha256, Payload), lowercase),
    binary:part(Hex, 0, 16).

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 归一化任意输入为合法 experience：只认 chat / workspace
%% （原子或对应 binary，忽略大小写差异不适用——binary 精确匹配），其余降级 chat。
-spec normalize(experience() | binary() | string() | term()) -> experience().
normalize(chat) ->
    chat;
normalize(workspace) ->
    workspace;
normalize(<<"workspace">>) ->
    workspace;
normalize(L) when is_list(L) ->
    try
        normalize(unicode:characters_to_binary(L))
    catch
        _:_ -> chat
    end;
normalize(_) ->
    chat.

-spec to_binary(experience()) -> binary().
to_binary(chat) ->
    <<"chat">>;
to_binary(workspace) ->
    <<"workspace">>.

%% @doc 应用发布版本（binary）。取 OTP application vsn（Makefile 从 VERSION
%% 文件注入），缺失/异常时 <<"unknown">>——镜像 agent_card_handler:version/0。
-spec app_vsn() -> binary().
app_vsn() ->
    case application:get_key(imboy, vsn) of
        {ok, V} when is_list(V) -> unicode:characters_to_binary(V);
        {ok, V} when is_binary(V) -> V;
        _ -> <<"unknown">>
    end.
