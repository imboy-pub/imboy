-module(wechat_mini_auth_handler).
-moduledoc "微信小程序认证 HTTP 适配层。".

-behaviour(cowboy_rest).

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
        ip => elib_req:get_client_ip(Req0),
        %% user.reg_cosv 的列注释就是「客户端操作系统版本」。小程序侧目前
        %% 不发该字段，这里退而取请求 UA —— 微信客户端 UA 里含真实
        %% 系统与版本（如 "iPhone; CPU iPhone OS 15_0"）。**绝不**让它落到
        %% normalize 的 "perf-test" 占位值：那会把内部压测标记写进
        %% 真实家长账号，运营侧再也分不清谁是压测数据。
        reg_cosv => client_os_hint(Req0)
    },
    case wechat_mini_auth_logic:wechat_mini_login(Params) of
        {ok, Payload} ->
            elib_response:success_rfc3339(Req0, Payload, <<"登录成功"/utf8>>);
        {error, Reason} ->
            login_error(Req0, Reason)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc 客户端系统线索（落 user.reg_cosv）。
%% 优先取请求 UA（微信客户端 UA 含真实系统版本）；UA 缺失时给
%% <<"unknown">> 而不是空串，便于运营侧区分「没采到」与「旧数据」。
%% 截断到 300 字节：该列是 text 无长度约束，但不接受无界的外部输入。
-define(REG_COSV_MAX, 300).
-spec client_os_hint(cowboy_req:req()) -> binary().
client_os_hint(Req) ->
    case cowboy_req:header(<<"user-agent">>, Req, <<>>) of
        <<>> ->
            <<"unknown">>;
        Ua when is_binary(Ua) ->
            truncate_utf8(Ua, ?REG_COSV_MAX)
    end.

-spec truncate_utf8(binary(), pos_integer()) -> binary().
truncate_utf8(Bin, Max) when byte_size(Bin) =< Max ->
    Bin;
truncate_utf8(Bin, Max) ->
    case unicode:characters_to_binary(binary:part(Bin, 0, Max)) of
        T when is_binary(T) -> T;
        _ -> <<"unknown">>
    end.

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
    elib_response:error(Req, <<"登录服务未配置"/utf8>>, ?ERR_WECHAT_PROVIDER_UNCONFIGURED);
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
