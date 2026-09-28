-module(moya_subscribe_logic).
%%%
% 墨芽订阅消息下发编排层（W3 服务端依赖）
% Moya subscribe-message orchestration
%
% 「老师发布回评 → 家长收到微信订阅消息」的编排层：
%   - report/2：客户端 requestSubscribeMessage 授权结果上报落库
%     （白名单过滤：只接受当前配置的模板 ID）
%   - notify_review_published/1：发布成功后对学员的可看回评监护人逐个
%     「查额度 → 反查 openid → 抢额度 → 外呼微信」，可测的同步版本
%   - notify_review_published_async/1：spawn 包装，给 HTTP 层在发布成功
%     响应后调用，绝不阻塞/影响发布主流程
%
% 总原则：一切失败静默（只记日志），绝不影响发布主流程——通知是增益
% 不是义务。每个环节（无额度 / 无微信身份 / 并发已消费 / 微信拒收）
% 都是该家长「跳过」而非整链失败。
%
% PII 纪律：openid 只在 find_subject_by_uid 与 subscribe_send 两点之间
% 内联传递，**绝不入日志**（日志只记 uid / submission / error 原子）；
% async 包装的 catch 也不落 stacktrace——帧参数可能携带 openid。
%
% 配置键（config_ds）：
%   wechat_mini_subscribe_template_id  单模板 ID 二进制；未配置/非法 =
%                                       上报宽容返回 0、下发 skipped
%   wechat_mini_subscribe_data          消息值模板 map（可选，见 build_data/1）
%%%

-export([
    report/2,
    notify_review_published_async/1,
    notify_review_published/1,
    %% 纯函数导出供 eunit 直测（thing 类字段 20 字符截断规则）
    truncate_utf8/2
]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% thing 类字段微信限 20 个 unicode 字符（超长直接 errcode 拒发）
-define(THING_MAX_CHARS, 20).

%% 默认消息值模板（平铺形态；发送前统一包成微信要求的嵌套 value 结构）
-define(DEFAULT_DATA_TEMPLATE, #{
    <<"thing1">> => <<"{learner_name}的《{task_title}》已有点评"/utf8>>
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 客户端授权上报：只接受白名单内的模板，每个保留模板落一行 pending。
%% 防御口径（宽容，绝不打扰用户）：
%%   - 配置未给模板 ID → {ok, 0}（此时上报无意义，不报错）
%%   - 入参元素非 binary / 空串 / 超 64 字节 → 直接丢弃不炸
%%   - 过滤后为空（全不在白名单）→ {ok, 0}
%%   - 整个入参不是 list → {error, invalid_templates}（调用方契约破裂）
%% 落库失败：记 ERROR 后宽容返回 {ok, 0}——客户端重试会造成过授权，
%% 丢失一次授权比打扰用户代价低（与 insert_grants 幂等性注释同权衡）。
-spec report(integer(), [binary()]) -> {ok, non_neg_integer()} | {error, invalid_templates}.
report(Uid, TemplateIds) when is_list(TemplateIds) ->
    case configured_template() of
        undefined ->
            {ok, 0};
        TemplateId ->
            Kept = lists:usort([
                T
             || T <- TemplateIds,
                is_binary(T),
                T =/= <<>>,
                byte_size(T) =< 64,
                T =:= TemplateId
            ]),
            report_insert(Uid, Kept)
    end;
report(_Uid, _NotAList) ->
    {error, invalid_templates}.

-spec report_insert(integer(), [binary()]) -> {ok, non_neg_integer()}.
report_insert(_Uid, []) ->
    {ok, 0};
report_insert(Uid, Kept) ->
    case moya_subscribe_repo:insert_grants(Uid, Kept) of
        {ok, Count} ->
            {ok, Count};
        {error, Reason} ->
            ?LOG_ERROR("moya subscribe report insert error uid=~p reason=~p", [Uid, Reason]),
            {ok, 0}
    end.

%% @doc 发布成功后的异步下发入口（HTTP 层在发布成功响应后调用）。
%% spawn + catch-all：任何异常只记日志（Class:Reason，不落 stacktrace——
%% 帧参数可能携带 openid），调用方拿到的恒为 ok。
-spec notify_review_published_async(integer()) -> ok.
notify_review_published_async(SubmissionId) ->
    _ = spawn(fun() ->
        try
            _ = notify_review_published(SubmissionId),
            ok
        catch
            Class:Reason ->
                ?LOG_ERROR("moya subscribe notify crashed submission=~p ~p:~p", [
                    SubmissionId, Class, Reason
                ])
        end
    end),
    ok.

%% @doc 下发编排（同步、可测）：返回 {ok, 实际通知人数 | skipped}。
%% 逐个监护人链路：pending_grant（无额度跳过）→ find_subject_by_uid
%% （无微信身份/查失败跳过）→ consume_grant（并发已被消费跳过）→
%% subscribe_send（结果仅记日志：成功 INFO 记 uid+submission，
%% 失败 WARNING 记 error 原子；**绝不记 openid**）。
-spec notify_review_published(integer()) -> {ok, non_neg_integer() | skipped}.
notify_review_published(SubmissionId) ->
    case configured_template() of
        undefined ->
            ?LOG_INFO("moya subscribe template unconfigured, skip notify submission=~p", [
                SubmissionId
            ]),
            {ok, skipped};
        TemplateId ->
            notify_with_template(TemplateId, SubmissionId)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 配置模板 ID 读取与防御：非空 binary 且 <= 64 字节才算已配置
%% （config 写坏不给下发，fail-closed 到 skipped）
-spec configured_template() -> binary() | undefined.
configured_template() ->
    case config_ds:env(wechat_mini_subscribe_template_id, <<>>) of
        T when is_binary(T), T =/= <<>>, byte_size(T) =< 64 -> T;
        _ -> undefined
    end.

-spec notify_with_template(binary(), integer()) -> {ok, non_neg_integer()}.
notify_with_template(TemplateId, SubmissionId) ->
    case moya_subscribe_repo:notification_context(SubmissionId) of
        {ok, Ctx} when is_map(Ctx) ->
            notify_guardians(TemplateId, SubmissionId, Ctx);
        {ok, undefined} ->
            ?LOG_INFO("moya subscribe context missing, skip notify submission=~p", [SubmissionId]),
            {ok, 0};
        {error, Reason} ->
            ?LOG_WARNING("moya subscribe context error submission=~p reason=~p", [
                SubmissionId, Reason
            ]),
            {ok, 0}
    end.

-spec notify_guardians(binary(), integer(), map()) -> {ok, non_neg_integer()}.
notify_guardians(TemplateId, SubmissionId, Ctx) ->
    LearnerId = maps:get(<<"learner_id">>, Ctx, 0),
    case moya_subscribe_repo:view_guardians(LearnerId) of
        {ok, Uids} when is_list(Uids) ->
            Data = build_data(Ctx),
            Page = detail_page(SubmissionId),
            Notified = lists:foldl(
                fun(Uid, Acc) ->
                    case notify_one(TemplateId, SubmissionId, Uid, Data, Page) of
                        true -> Acc + 1;
                        false -> Acc
                    end
                end,
                0,
                Uids
            ),
            {ok, Notified};
        {error, Reason} ->
            ?LOG_WARNING("moya subscribe guardians error learner=~p reason=~p", [
                LearnerId, Reason
            ]),
            {ok, 0}
    end.

%% 单个监护人的完整链路；true=已实际下发（send 成功）
-spec notify_one(binary(), integer(), integer(), map(), binary()) -> boolean().
notify_one(TemplateId, SubmissionId, Uid, Data, Page) ->
    case moya_subscribe_repo:pending_grant(Uid, TemplateId) of
        {ok, GrantId} when is_integer(GrantId) ->
            notify_with_grant(TemplateId, SubmissionId, Uid, GrantId, Data, Page);
        {ok, undefined} ->
            %% 无额度 = 家长未授权过（或额度已用完），静默跳过
            false;
        {error, Reason} ->
            ?LOG_WARNING("moya subscribe pending_grant error uid=~p reason=~p", [Uid, Reason]),
            false
    end.

-spec notify_with_grant(binary(), integer(), integer(), integer(), map(), binary()) -> boolean().
notify_with_grant(TemplateId, SubmissionId, Uid, GrantId, Data, Page) ->
    case sso_identity_repo:find_subject_by_uid(<<"wechat_mini">>, Uid) of
        {ok, [#{<<"subject">> := Openid} | _]} when is_binary(Openid), Openid =/= <<>> ->
            %% openid 只向下传递给 send，不进任何日志分支
            consume_and_send(TemplateId, SubmissionId, Uid, GrantId, Openid, Data, Page);
        {ok, []} ->
            %% 无微信身份（如手机号注册的家长）：无投递通道，额度留着
            false;
        {ok, _OtherRows} ->
            false;
        {error, Reason} ->
            ?LOG_WARNING("moya subscribe find_subject error uid=~p reason=~p", [Uid, Reason]),
            false
    end.

-spec consume_and_send(binary(), integer(), integer(), integer(), binary(), map(), binary()) ->
    boolean().
consume_and_send(TemplateId, SubmissionId, Uid, GrantId, Openid, Data, Page) ->
    case moya_subscribe_repo:consume_grant(GrantId, Uid, SubmissionId) of
        true ->
            do_send(TemplateId, SubmissionId, Uid, Openid, Data, Page);
        false ->
            %% 并发已被消费（两处发布同时触发）：本方让出，防双发
            false
    end.

-spec do_send(binary(), integer(), integer(), binary(), map(), binary()) -> boolean().
do_send(TemplateId, SubmissionId, Uid, Openid, Data, Page) ->
    case moya_wechat_client:subscribe_send(Openid, TemplateId, Data, Page) of
        {ok, sent} ->
            ?LOG_INFO("moya subscribe sent uid=~p submission=~p", [Uid, SubmissionId]),
            true;
        {error, Reason} ->
            %% 微信侧失败（网络/拒收/模板问题）：额度已消费不回滚（repo 注释
            %% 同语义），该家长本轮无通知，不影响其余监护人
            ?LOG_WARNING("moya subscribe send failed uid=~p submission=~p error=~p", [
                Uid, SubmissionId, Reason
            ]),
            false
    end.

%%%-------------------------------------------------------------------
%%% 消息内容构造
%%%-------------------------------------------------------------------

%% data 构造：config 的 wechat_mini_subscribe_data 为 map 时作值模板，
%% 否则用默认模板。模板支持两种形态（发送前统一归一为微信要求的嵌套）：
%%   平铺：#{<<"thing1">> => <<"{learner_name}的…">>}         （默认/简配）
%%   嵌套：#{<<"thing1">> => #{<<"value">> => <<"{learner_name}…">>}}（微信原生）
%% 每个 binary 值做 {learner_name}/{task_title} 占位符替换 + 20 字符截断。
-spec build_data(map()) -> map().
build_data(Ctx) ->
    Template =
        case config_ds:env(wechat_mini_subscribe_data, undefined) of
            M when is_map(M) -> M;
            _ -> ?DEFAULT_DATA_TEMPLATE
        end,
    LearnerName = bin(maps:get(<<"learner_name">>, Ctx, <<>>)),
    TaskTitle = bin(maps:get(<<"task_title">>, Ctx, <<>>)),
    maps:map(
        fun(_Key, Value) -> data_value(Value, LearnerName, TaskTitle) end,
        Template
    ).

-spec data_value(term(), binary(), binary()) -> term().
data_value(#{<<"value">> := Inner} = Value, LearnerName, TaskTitle) when is_binary(Inner) ->
    Value#{<<"value">> := render_value(Inner, LearnerName, TaskTitle, ?THING_MAX_CHARS)};
data_value(Bin, LearnerName, TaskTitle) when is_binary(Bin) ->
    #{<<"value">> => render_value(Bin, LearnerName, TaskTitle, ?THING_MAX_CHARS)};
data_value(Other, _LearnerName, _TaskTitle) ->
    Other.

%% 占位符替换 + thing 类字段长度收口（替换后再截断，终值不超限）
-spec render_value(binary(), binary(), binary(), pos_integer()) -> binary().
render_value(Value, LearnerName, TaskTitle, MaxChars) ->
    Replaced = binary:replace(Value, <<"{learner_name}">>, LearnerName, [global]),
    Replaced2 = binary:replace(Replaced, <<"{task_title}">>, TaskTitle, [global]),
    truncate_utf8(Replaced2, MaxChars).

%% @doc 按 unicode 字符数截断（thing 类字段微信限 20 字符）。
%% 超长时保留前 Max-1 个字符并追加省略号（U+2026，总计恰 Max 个字符）；
%% 非法 UTF-8 原样返回（微信侧自会拒发，不在此崩溃拖垮整链下发）。
-spec truncate_utf8(binary(), pos_integer()) -> binary().
truncate_utf8(Bin, MaxChars) when is_binary(Bin), is_integer(MaxChars), MaxChars > 0 ->
    case unicode:characters_to_list(Bin, utf8) of
        Chars when is_list(Chars) ->
            case length(Chars) =< MaxChars of
                true ->
                    Bin;
                false ->
                    Kept = lists:sublist(Chars, MaxChars - 1),
                    unicode:characters_to_binary(Kept ++ [16#2026], utf8)
            end;
        _ ->
            Bin
    end.

%% 跳转页：家长端提交详情页（TSID 与服务端其他 DTO 一致，十进制字符串）
-spec detail_page(integer()) -> binary().
detail_page(SubmissionId) ->
    <<
        "/packages/parent/submission-detail/submission-detail?id=",
        (integer_to_binary(SubmissionId))/binary,
        "&from=notify"
    >>.

%% 二进制归一：非 binary 值（脏数据）归空串，占位符替换不炸
-spec bin(term()) -> binary().
bin(B) when is_binary(B) ->
    B;
bin(_) ->
    <<>>.
