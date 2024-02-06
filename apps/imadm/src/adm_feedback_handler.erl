-module(adm_feedback_handler).
%%%
% adm_feedback 控制器模块
% adm_feedback controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        index ->
            {ok, Ajax} = imboy_req:get_int(ajax, Req0, -2),
            % imboy_log:info(["AjaxAjaxAjaxAjaxAjax: ", Ajax, ";  Ajax"]),
            index(Method, Ajax, Req0, State);
        reply ->
            reply(Method, Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

index(<<"GET">>, 1, Req0, _State) ->
    {Page, Size} = imboy_req:page_size(Req0),
    % Where = imboy_cnv:implode("", [<<"user_id=">>, CurrentUid]),
    % Where2 = <<"status > 0 AND ", Where/binary>>,
    Where = <<"status > -2">>,
    Column = <<"id as feedback_id, user_id, device_id, client_operating_system, client_operating_system_vsn, type, rating, contact_detail, body, attach, reply_count, status, updated_at, created_at, app_vsn">>,
    Payload = feedback_ds:page(Page, Size, Where, <<"id desc">>, Column),
    imboy_response:success(Req0, Payload);
index(<<"GET">>, _, Req0, State) ->
    {ok, Body} = imboy_dtl:template(feedback_index_dtl, [
        {attach_token, ""}
    ] ++ imboy_dtl:imadm_param(State), imadm),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_index_dtl.html"])),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).

reply(<<"POST">>, Req0, State) ->
    % Uid = imboy_hashids:uid_encode(CurrentUid),
    AdmUserId = maps:get(adm_user_id, State),
    Key = {adm_user_sample, AdmUserId},
    {true, {_, Nickname}} = adm_user_logic:find(AdmUserId, <<"id,nickname">>, Key),
    % replier_user_id
    PostVals = imboy_req:post_params(Req0),
    % FeedbackId = proplists:get_value(<<"feedback_id">>, PostVals),
    % FeedbackId = imboy_req:get_int(<<"FeedbackId">>, Req0, 0),
    {ok, FeedbackId } = case string:to_integer(proplists:get_value(<<"feedback_id">>, PostVals)) of
        {error, _} ->
            {ok, 0};
        {Val2, _} ->
            {ok, Val2}
    end,
    % ?LOG(["FeedbackId", FeedbackId, PostVals]),
    if
        is_integer(FeedbackId), FeedbackId > 0 ->
            feedback_ds:add_reply(#{
                <<"feedback_id">> => FeedbackId
                , <<"feedback_reply_pid">> => proplists:get_value(<<"feedback_reply_pid">>, PostVals, 0)
                , <<"replier_user_id">> => AdmUserId
                , <<"replier_name">> => Nickname
                , <<"body">> => proplists:get_value(<<"body">>, PostVals, "")
                , <<"created_at">> => imboy_dt:utc(millisecond)}),
            imboy_response:success(Req0, PostVals, "success.");
        true ->
            imboy_response:error(Req0)
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
