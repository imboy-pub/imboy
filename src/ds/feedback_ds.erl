-module(feedback_ds).
%%%
% feedback 业务逻辑模块
% feedback business logic module
%%%

-export ([add/10]).
-export ([remove/2]).

-export([add_reply/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%%% add方法
%%% 新增用户反馈
-spec add(integer(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
% feedback_ds:add(Uid, Did, COS, COSV, AppVsn, ContactDetail, Body, Attach)
add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach) ->
    FeedbackMd5 = imboy_hasher:md5(imboy_cnv:implode("", [
        Uid, Did, AppVsn, Type, Body
        ])),

    % 使用安全的参数化查询，避免SQL注入
    Count = imboy_pg:pluck_value(<<"feedback">>,
       <<"count(*)">>,
       #{<<"feedback_md5">> => FeedbackMd5}, #{}, 0),
    if Count > 0 ->
            ok;
        true ->
            feedback_repo:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach, FeedbackMd5)
    end.

-spec remove(integer(), binary()) -> ok.
remove(Uid, FeedbackId) ->
    % 状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）
    % 使用安全的参数化查询，避免SQL注入
    Where = <<"user_id = $1 AND id = $2">>,
    imboy_pg:update(feedback_repo:tablename(), #{
        <<"status">> => <<"-1">>,
        <<"updated_at">> => imboy_dt:now()
    }, Where, [Uid, FeedbackId]),
    % feedback_repo:delete(Uid, FeedbackId),
    % Key = {user_device_name, Uid, FeedbackId},
    % imboy_cache:flush(Key),
    ok.


% feedback_ds:add_reply(#{feedback_id => 1, feedback_reply_pid => 0, replier_user_id => 1, replier_name => <<"sss">>, body => "", created_at => imboy_dt:now()})
-spec add_reply(any()) -> any().
add_reply(Data) ->
    FeedbackId = maps:get(<<"feedback_id">>, Data),
    Tb = feedback_reply_repo:tablename(),
    {Sql, Params} = imboy_pg_sql:insert(Tb, Data),
    imboy_pg:execute(Sql, Params),
    % 使用安全的参数化查询，避免SQL注入
    imboy_pg:update(feedback_repo:tablename(), #{
        <<"status">> => <<"2">>,
        <<"reply_count">> => {raw, <<"reply_count + 1">>},
        <<"updated_at">> => imboy_dt:now()
    }, <<"id = $1">>, [FeedbackId]),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

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
