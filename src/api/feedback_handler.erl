-module(feedback_handler).

%%%
% feedback 控制器模块
% feedback controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

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
        <<"id as feedback_id, device_id, type, rating, contact_detail, "
          "body, ",
          "(select array_agg(replace(item, '\\\/', '/') ) from jsonb_array_elem"
          "ents_text(coalesce(attach::jsonb, '[]'::jsonb)) as item) as "
          "attach, ",
          "reply_count, status, updated_at, created_at, app_vsn">>,
    Tb = feedback_repo:tablename(),
    {ok, Payload} = elib_pg:page_with_total(Tb, Column, Where, <<"id desc">>, Page, Size),
    elib_response:success(Req0, Payload).

%% @doc 用户反馈回复分页列表
%% 获取指定反馈的回复记录，支持分页查询
%%
%% @param Req0 Cowboy请求对象，包含 feedback_id 和分页参数
%% @param _State 状态映射
%% @return 返回包含分页数据的响应
%% @end
-spec page_reply(cowboy_req:req(), map()) -> cowboy_req:req().
page_reply(Req0, _State) ->
    Def = 0,
    case elib_param:int(feedback_id, Req0, Def) of
        {ok, 0} ->
            elib_response:error(Req0, <<"反馈ID必须是整数"/utf8>>);
        {ok, FeedbackId} ->
            % CurrentUid = auth_ds:current_uid(State),
            {Page, Size} = elib_param:page(Req0),
            Where = #{feedback_id => FeedbackId},

            Column =
                <<"id as feedback_reply_id, feedback_id, feedback_reply_pid, replier_us"
                  "er_id, replier_name, body, status, updated_at, created_at">>,
            Tb = feedback_reply_repo:tablename(),
            {ok, Payload} = elib_pg:page_with_total(Tb, Column, Where, <<"id desc">>, Page, Size),
            elib_response:success(Req0, Payload)
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
    % Uid = elib_hashids:encode(CurrentUid),
    COS = cowboy_req:header(<<"cos">>, Req0),
    AppVsn = cowboy_req:header(<<"vsn">>, Req0),
    Did = cowboy_req:header(<<"did">>, Req0),

    PostVals = elib_param:post(Req0),
    Type = maps:get(<<"type">>, PostVals, <<>>),
    Rating = maps:get(<<"rating">>, PostVals, <<"0">>),
    ContactDetail = maps:get(<<"contact_detail">>, PostVals, <<>>),
    % 支持两种字段名：content 和 description
    Description = case maps:get(<<"content">>, PostVals, undefined) of
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
            feedback_ds:add(CurrentUid,
                            Did,
                            COS,
                            COSV,
                            AppVsn,
                            ec_cnv:to_binary(Type),
                            ec_cnv:to_binary(Rating),
                            ec_cnv:to_binary(ContactDetail),
                            ec_cnv:to_binary(Description),
                            Attach),
            elib_response:success(Req0)
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
            feedback_ds:remove(CurrentUid, FeedbackId),
            elib_response:success(Req0)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).

%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
