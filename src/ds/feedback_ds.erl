-module(feedback_ds).
%%%
% feedback 领域服务模块
% feedback domain service 缩写
%%%

%% @doc 新增用户反馈
%% 添加用户反馈，通过 MD5 去重避免重复提交
%% @param Uid 用户ID
%% @param Did 设备ID
%% @param COS 客户端操作系统
%% @param COSV 客户端操作系统版本
%% @param AppVsn 应用版本
%% @param Type 反馈类型
%% @param Rating 评分
%% @param ContactDetail 联系方式
%% @param Body 反馈内容
%% @param Attach 附件信息
%% @returns ok | {ok, non_neg_integer()}
-export ([add/10]).

%% @doc 删除用户反馈
%% 软删除反馈，将状态更新为 -1
%% @param Uid 用户ID
%% @param FeedbackId 反馈ID
%% @returns ok
-export ([remove/2]).

%% @doc 添加反馈回复
%% 为反馈添加回复，并更新反馈状态
%% @param Data 回复数据映射
%% @returns ok
-export([add_reply/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%%% add方法
%%% 新增用户反馈
-spec add(integer(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary()) ->
    ok | {ok, non_neg_integer()}.
% feedback_ds:add(Uid, Did, COS, COSV, AppVsn, ContactDetail, Body, Attach)
add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach) ->
    FeedbackMd5 = elib_hasher:md5(elib_cnv:implode("", [
        Uid, Did, AppVsn, Type, Body
        ])),

    % 使用安全的参数化查询，避免SQL注入
    Count = elib_pg:pluck_value(<<"feedback">>,
       <<"count(*)">>,
       #{<<"feedback_md5">> => FeedbackMd5}, #{}, 0),
    if Count > 0 ->
            ok;
        true ->
            case feedback_repo:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach, FeedbackMd5) of
                {ok, _} -> ok;
                {error, Reason} -> ?ERROR_LOG([feedback_add_failed, Uid, Reason])
            end,
            ok
    end.

-spec remove(integer(), integer()) -> ok.
remove(Uid, FeedbackId) ->
    % 状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）
    % 使用安全的参数化查询，避免SQL注入
    Where = <<"user_id = $1 AND id = $2">>,
    case elib_pg:update(feedback_repo:tablename(), #{
        <<"status">> => -1,
        <<"updated_at">> => elib_dt:now()
    }, Where, [Uid, FeedbackId]) of
        {ok, _} -> ok;
        {error, Reason} -> ?ERROR_LOG([feedback_status_update_failed, Uid, FeedbackId, Reason])
    end,
    % feedback_repo:delete(Uid, FeedbackId),
    % Key = {user_device_name, Uid, FeedbackId},
    % imboy_cache:flush(Key),
    ok.


% feedback_ds:add_reply(#{feedback_id => 1, feedback_reply_pid => 0, replier_user_id => 1, replier_name => <<"sss">>, body => "", created_at => elib_dt:now()})
-spec add_reply(map()) -> ok.
add_reply(Data) ->
    FeedbackId = maps:get(<<"feedback_id">>, Data),
    Tb = feedback_reply_repo:tablename(),
    {Sql, Params} = elib_pg_sql:insert(Tb, Data, <<"">>),
    case elib_pg:execute(Sql, Params) of
        {ok, _} -> ok;
        {error, Reason1} -> ?ERROR_LOG([feedback_reply_add_failed, FeedbackId, Reason1])
    end,
    % 使用安全的参数化查询，避免SQL注入
    case elib_pg:update(feedback_repo:tablename(), #{
        <<"status">> => 2,
        <<"reply_count">> => {raw, <<"reply_count + 1">>},
        <<"updated_at">> => elib_dt:now()
    }, <<"id = $1">>, [FeedbackId]) of
        {ok, _} -> ok;
        {error, Reason2} -> ?ERROR_LOG([feedback_status_update_failed, FeedbackId, Reason2])
    end,
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================
