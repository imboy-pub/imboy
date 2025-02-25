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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

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

    Count = imboy_db:pluck(<<"feedback">>,
       <<"feedback_md5 = '", FeedbackMd5/binary, "'">>,
       <<"count(*)">>,
       0),
    if Count > 0 ->
            ok;
        true ->
            feedback_repo:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach, FeedbackMd5)
    end.

-spec remove(integer(), binary()) -> ok.
remove(Uid, FeedbackId) ->
    % 状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）
    Where = imboy_cnv:implode("", ["user_id = ", Uid," AND id = ", FeedbackId]),
    imboy_db:update(feedback_repo:tablename(), Where, [
        {<<"status">>, <<"-1">>}
        , {<<"updated_at">>, imboy_dt:now()}
    ]),
    % feedback_repo:delete(Uid, FeedbackId),
    % Key = {user_device_name, Uid, FeedbackId},
    % imboy_cache:flush(Key),
    ok.


% feedback_ds:add_reply(#{feedback_id => 1, feedback_reply_pid => 0, replier_user_id => 1, replier_name => <<"sss">>, body => "", created_at => imboy_dt:now()})
add_reply(Data) ->
    FeedbackId = maps:get(<<"feedback_id">>, Data),
    Tb = feedback_reply_repo:tablename(),
    imboy_db:insert_into(Tb, Data),
    Where = <<"id = ", (ec_cnv:to_binary(FeedbackId))/binary>>,
    KV = [
        % 状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）'
        {<<"status">>, <<"2">>}
        , {<<"reply_count">>, {raw, <<"reply_count + 1">>}}
        , {<<"updated_at">>, imboy_dt:now()}
    ],
    imboy_db:update(feedback_repo:tablename(), Where, KV),
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
