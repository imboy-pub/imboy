-module(moya_auth_handler).
%%%
% 墨芽教学认证 HTTP 适配层（微信小程序登录）
% Thin HTTP adapter for moya wechat-mini login
%
% POST /api/v1/auth/wechat-mini/login（免 Bearer，open 路由）
% 错误码映射（STEP-04 error-codes.md 5400 段）：
%   invalid_code(参数) → 422 | missing/invalid code → 422/5402
%   provider_unconfigured → 5403 | login_failed → 5401 | identity_none → 5404
%   account_quota_exceeded → 402（复用全局授权上限码，与 passport/oidc 注册同款）
%
% 2026-09-20（试点方案 B）：首登已改为自动开户，identity_none 正常流程下不再
% 产生；该分支保留为兼容（服务端将来若关闭自动开户仍会用到）。
%%%

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(wechat_mini_login, Req, _State) ->
    wechat_mini_login(Req);
handle_action(false, Req, _State) ->
    Req.

%% @doc 微信小程序登录
-spec wechat_mini_login(cowboy_req:req()) -> cowboy_req:req().
wechat_mini_login(Req0) ->
    PostVals = elib_param:post(Req0),
    Params = #{
        code => maps:get(<<"code">>, PostVals, <<>>),
        device_id => maps:get(<<"device_id">>, PostVals, <<>>),
        %% 首登自动开户会落 user.reg_ip：取真实客户端 IP，勿留占位值
        ip => elib_req:get_client_ip(Req0)
    },
    case moya_auth_logic:wechat_mini_login(Params) of
        {ok, Payload} ->
            elib_response:success_rfc3339(Req0, Payload, <<"登录成功"/utf8>>);
        {error, Reason} ->
            login_error(Req0, Reason)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec login_error(cowboy_req:req(), atom()) -> cowboy_req:req().
login_error(Req, missing_code) ->
    elib_response:error(Req, <<"缺少登录凭证"/utf8>>, ?ERR_MISSING_PARAM);
login_error(Req, invalid_code) ->
    %% 长度/格式不合规（5..128）：参数错误而非微信侧拒绝
    elib_response:error(Req, <<"登录凭证格式不正确"/utf8>>, ?ERR_PARAM_INVALID);
login_error(Req, code_invalid) ->
    %% 微信侧 errcode（含 40163 code 重放）折叠为同一响应（T11）
    elib_response:error(Req, <<"微信登录凭证无效或已使用"/utf8>>, ?ERR_WECHAT_CODE_INVALID);
login_error(Req, provider_unconfigured) ->
    elib_response:error(Req, <<"登录服务未配置"/utf8>>, ?ERR_TEACHING_PROVIDER_UNCONFIGURED);
login_error(Req, login_failed) ->
    elib_response:error(Req, <<"微信登录失败"/utf8>>, ?ERR_WECHAT_LOGIN_FAILED);
login_error(Req, identity_none) ->
    elib_response:error(Req, <<"该微信未绑定教学账号，请联系机构"/utf8>>, ?ERR_TEACHING_IDENTITY_NONE);
login_error(Req, account_quota_exceeded) ->
    %% 首次开户撞上 License 用户数上限：**永久**条件，重试不会成功 ⇒
    %% 文案必须指向「找管理员」，且客户端侧按 402 关掉重试按钮。
    %% 不透 quota_guard 的原始 msg（含 License 细节）。
    elib_response:error(
        Req,
        <<"用户数已达授权上限，请联系机构管理员"/utf8>>,
        ?ERR_PAYMENT_REQUIRED
    );
login_error(Req, _Other) ->
    elib_response:error(Req, <<"微信登录失败"/utf8>>, ?ERR_WECHAT_LOGIN_FAILED).
