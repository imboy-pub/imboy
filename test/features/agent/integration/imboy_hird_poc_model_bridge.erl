%%% @doc IMBOY_HIRD_AUTH_REPLAY_POC_V1 — 模型返回 → Hirð Decision ADT 的
%%% 宿主侧桥（C5 数据流「模型返回结构 → 工具选择与参数」的落点）。
%%%
%%% 下一轮真实 Provider 授权到位后经 `imboy_llm_registry` 即插即用；本轮
%%% 全部离线——fake provider（实现 `imboy_llm` behaviour），零真实调用、
%%% 零密钥读取。
%%%
%%% 决策协议（下一轮约定；OpenAI 兼容 chat 的 content 承载 JSON）：
%%%   {"action":"call_ws","ws_id":912001}  → 请求业务工具（仍经授权链裁决）
%%%   {"action":"finish","answer":"..."}   → 最终输出，不再执行工具
%%%
%%% fail-closed：非法 JSON / 未知 action / 非法参数一律 `{error, _}`；
%%% handler 面统一转 `{hird_exn, {tool_denied, Reason}}` 域错误（与授权
%%% deny 同形状，ADR-016）——模型无法通过畸形输出把请求伪装成合法决策。
%%% 资源归属（org/ws scope）不在本层——由授权链裁决，职责单一边界。
-module(imboy_hird_poc_model_bridge).

-export([decision_from_content/1, make_decide_handler/2]).

%% 协议动作白名单：模型只能表达这两种意图——「请求签名外的工具」在协议
%% 层即被拒绝（与 Hirð Decision 封闭 ADT 构成双层类型/协议拒绝面）。
-define(ACTIONS, #{<<"call_ws">> => call_ws, <<"finish">> => finish}).

%% @doc content JSON → Decision ADT（Erlang 形状 {call_ws, Id}|{finish, A}）。
-spec decision_from_content(binary()) ->
    {call_ws, pos_integer()} | {finish, binary()} | {error, term()}.
decision_from_content(Content) when is_binary(Content) ->
    try jsone:decode(Content) of
        %% action 值非 binary 时 maps:find 同样 miss → unknown_action 覆盖。
        #{<<"action">> := ActionBin} = Obj ->
            case maps:find(ActionBin, ?ACTIONS) of
                {ok, Action} -> decision(Action, Obj);
                error -> {error, {unknown_action, ActionBin}}
            end;
        _ ->
            {error, action_missing}
    catch
        _:_ -> {error, bad_json}
    end;
decision_from_content(_Other) ->
    {error, bad_content}.

decision(call_ws, #{<<"ws_id">> := WsId}) when is_integer(WsId), WsId > 0 ->
    {call_ws, WsId};
decision(call_ws, _Obj) ->
    {error, bad_ws_id};
decision(finish, #{<<"answer">> := Answer}) when is_binary(Answer) ->
    {finish, Answer};
decision(finish, _Obj) ->
    {error, bad_answer}.

%% @doc model_decide handler 工厂（hird registry/threaded handler 形状）。
%% Provider：`{mod, Module, Opts}`（测试/直注）或 `{name, Bin}`（生产经
%% `imboy_llm_registry`）。
make_decide_handler(Uid, {mod, Mod, Opts}) ->
    decide_fun(fun() -> Mod:chat(Uid, messages(), Opts) end);
make_decide_handler(Uid, {name, ProviderName}) ->
    decide_fun(fun() ->
        case imboy_llm_registry:lookup(ProviderName) of
            {ok, #{module := Mod, opts := Opts}} ->
                Mod:chat(Uid, messages(), Opts);
            undefined ->
                {error, provider_not_found}
        end
    end).

decide_fun(ChatFun) ->
    fun(#{step := Step}, _Hs) ->
        case chat_to_decision(ChatFun, Step) of
            {call_ws, _WsId} = D ->
                D;
            {finish, _Answer} = D ->
                D;
            {error, Reason} ->
                throw({hird_exn, {tool_denied, reason_bin({decision_rejected, Reason})}})
        end
    end.

chat_to_decision(ChatFun, _Step) ->
    case ChatFun() of
        {ok, Resp} ->
            case extract_content(Resp) of
                {ok, Content} -> decision_from_content(Content);
                {error, R} -> {error, R}
            end;
        {error, R} ->
            {error, {provider_error, R}}
    end.

extract_content(#{<<"choices">> := [#{<<"message">> := #{<<"content">> := C}} | _]}) when
    is_binary(C)
->
    {ok, C};
extract_content(_Resp) ->
    {error, bad_response_shape}.

%% 协议 prompt（下一轮真实 prompt 的雏形；本轮 fake provider 不消费它）。
messages() ->
    Protocol =
        <<
            "Return exactly one JSON decision per turn: "
            "{\"action\":\"call_ws\",\"ws_id\":N} to read a workspace, or "
            "{\"action\":\"finish\",\"answer\":\"...\"} to deliver the final answer. "
            "No other actions exist."
        >>,
    [#{<<"role">> => <<"system">>, <<"content">> => Protocol}].

reason_bin(T) when is_tuple(T) ->
    unicode:characters_to_binary(io_lib:format("~0p", [T]));
reason_bin(B) when is_binary(B) ->
    B.
