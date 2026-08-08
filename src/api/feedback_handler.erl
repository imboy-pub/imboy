-module(feedback_handler).

%% Thin HTTP adapter for the ops_governance feedback boundary.

%%%
% feedback 控制器模块
% feedback controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State0),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    % ?DEBUG_LOG([Action, State]),
    Req1 =
        case Action of
            page ->
                page(Req0, State);
            add ->
                add(Req0, State);
            remove ->
                remove(Req0, State);
            % change ->
            %     change(Req0, State);
            % reply ->
            %     reply(Req0, State);
            page_reply ->
                page_reply(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 用户反馈分页列表
%% 获取当前用户的反馈记录，支持分页查询
%%
%% @param Req0 Cowboy请求对象，包含分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含分页数据的响应
%% @end
-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    Where = #{user_id => CurrentUid, status => {op, <<">">>, 0}},
    Column =
        <<
            "id as feedback_id, device_id, type, rating, contact_detail, "
            "body, ",
            "(select array_agg(replace(item, '\\\/', '/') ) from jsonb_array_elem"
            "ents_text(coalesce(attach::jsonb, '[]'::jsonb)) as item) as "
            "attach, ",
            "reply_count, status, updated_at, created_at, app_vsn"
        >>,
    {ok, Payload} = feedback_logic:page(Column, Where, <<"id desc">>, Page, Size),
    elib_response:success(Req0, Payload).

%% @doc 用户反馈回复分页列表
%% 获取指定反馈的回复记录，支持分页查询
%%
%% @param Req0 Cowboy请求对象，包含 feedback_id 和分页参数
%% @param _State 状态映射
%% @return 返回包含分页数据的响应
%% @end
-spec page_reply(cowboy_req:req(), map()) -> cowboy_req:req().
page_reply(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Def = 0,
    case elib_param:int(feedback_id, Req0, Def) of
        {ok, 0} ->
            elib_response:error(Req0, <<"反馈ID必须是整数"/utf8>>);
        {ok, FeedbackId} ->
            {Page, Size} = elib_param:page(Req0),
            Where = #{feedback_id => FeedbackId},

            Column =
                <<
                    "id as feedback_reply_id, feedback_id, feedback_reply_pid, replier_us"
                    "er_id, replier_name, body, status, updated_at, created_at"
                >>,
            case
                feedback_logic:page_reply(
                    CurrentUid, FeedbackId, Column, Where, <<"id desc">>, Page, Size
                )
            of
                {ok, Payload} ->
                    Payload2 = normalize_reply_payload(Payload),
                    elib_response:success(Req0, Payload2);
                {error, forbidden} ->
                    elib_response:error(Req0, <<"无权限查看该反馈的回复"/utf8>>, ?ERR_FORBIDDEN);
                {error, not_found} ->
                    elib_response:error(Req0, <<"反馈不存在"/utf8>>, ?ERR_NOT_FOUND)
            end
    end.

%% @doc 添加用户反馈
%% 用户提交反馈信息，包含类型、评分、联系方式和描述
%%
%% @param Req0 Cowboy请求对象，包含反馈数据和设备信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State, 0),
    COS = cowboy_req:header(<<"cos">>, Req0),
    AppVsn = cowboy_req:header(<<"vsn">>, Req0),
    Did = cowboy_req:header(<<"did">>, Req0),

    PostVals = elib_param:post(Req0),
    % 空串视为未提供（旧客户端发送 "" 使 maps:get 默认值失效），
    % 回落合法默认值：type=bugReport / rating=neutral；
    % 否则撞 feedback 表 CHECK 约束（chk_feedback_type / chk_feedback_rating），
    % 旧默认 <<"0">> 本身也不在 chk_feedback_rating 白名单内，必插入失败。
    TypeBin = ec_cnv:to_binary(maps:get(<<"type">>, PostVals, <<"bugReport">>)),
    TypeBin1 =
        case TypeBin of
            <<>> -> <<"bugReport">>;
            _ -> TypeBin
        end,
    RatingBin = ec_cnv:to_binary(maps:get(<<"rating">>, PostVals, <<"neutral">>)),
    RatingBin1 =
        case RatingBin of
            <<>> -> <<"neutral">>;
            _ -> RatingBin
        end,
    ContactDetail = maps:get(<<"contact_detail">>, PostVals, <<>>),
    % 支持两种字段名：content 和 description
    Description =
        case maps:get(<<"content">>, PostVals, undefined) of
            undefined -> maps:get(<<"description">>, PostVals, <<>>);
            Content -> Content
        end,
    Dcreenshot = maps:get(<<"screenshot">>, PostVals, []),
    Attach = jsone:encode(Dcreenshot, [native_utf8]),

    % 验证反馈内容不为空
    case byte_size(Description) of
        0 ->
            elib_response:error(Req0, <<"反馈内容不能为空"/utf8>>);
        _ ->
            COSV = maps:get(<<"sys_version">>, PostVals, <<>>),
            case
                feedback_logic:add(
                    CurrentUid,
                    Did,
                    COS,
                    COSV,
                    AppVsn,
                    TypeBin1,
                    RatingBin1,
                    ec_cnv:to_binary(ContactDetail),
                    ec_cnv:to_binary(Description),
                    Attach
                )
            of
                ok ->
                    elib_response:success(Req0);
                {error, Reason} ->
                    % 插入失败必须返回错误：此前忽略返回值无条件 success，
                    % 客户端 showSuccess 假成功，反馈数据从未入库。
                    ?ERROR_LOG([feedback_add_failed, CurrentUid, Reason]),
                    elib_response:error(
                        Req0,
                        <<"反馈提交失败，请稍后重试"/utf8>>,
                        ?ERR_SERVER_ERROR
                    )
            end
    end.

%% @doc 删除用户反馈
%% 用户删除指定的反馈记录
%%
%% @param Req0 Cowboy请求对象，包含 feedback_id 参数
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec remove(cowboy_req:req(), map()) -> cowboy_req:req().
remove(Req0, State) ->
    Def = 0,
    case elib_param:int(feedback_id, Req0, Def) of
        {ok, 0} ->
            elib_response:error(Req0, <<"反馈ID必须是整数"/utf8>>);
        {ok, FeedbackId} ->
            CurrentUid = auth_ds:current_uid(State),
            feedback_logic:remove(CurrentUid, FeedbackId),
            elib_response:success(Req0)
    end.

%% ===================================================================
%% 数据规范化函数（ID编码）
%% ===================================================================

%% @doc 规范化回复分页数据（编码ID字段）
-spec normalize_reply_payload(map()) -> map().
normalize_reply_payload(Payload) ->
    List = maps:get(list, Payload, maps:get(items, Payload, [])),
    List2 = [normalize_reply(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

%% @doc 规范化单条回复数据（编码ID字段）
-spec normalize_reply(map()) -> map().
normalize_reply(Reply) ->
    Reply.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
