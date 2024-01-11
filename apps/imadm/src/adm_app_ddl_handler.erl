-module(adm_app_ddl_handler).
%%%
% adm_app_ddl 控制器模块
% adm_app_ddl controller module
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
        save ->
            save(Method, Req0, State);
        delete ->
            delete(Method, Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
index(<<"GET">>, 1, Req0, _State) ->
    {Page, Size} = imboy_req:page_size(Req0),
    Where = <<"1=1">>,
    Column = <<"id, ddl, down_ddl,old_vsn,new_vsn,type,status,updated_at,created_at">>,
    Payload = app_ddl_ds:page(Page, Size, Where, <<"id desc">>, Column),
    imboy_response:success(Req0, Payload);
index(<<"GET">>, _, Req0, State) ->
    {ok, Body} = imboy_dtl:template(app_ddl_index_dtl, [
        {attach_token, ""}
    ] ++ imboy_dtl:imadm_param(State), imadm),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_index_dtl.html"])),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).


save(<<"POST">>, Req0, State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),
    AdmUserId = maps:get(adm_user_id, State),

    PostVals = imboy_req:post_params(Req0),
    Type = proplists:get_value(<<"type">>, PostVals, 3),
    NewVsn = proplists:get_value(<<"new_vsn">>, PostVals, 0),
    OldVsn = proplists:get_value(<<"old_vsn">>, PostVals, 0),
    Status = proplists:get_value(<<"status">>, PostVals, 0),
    Ddl = proplists:get_value(<<"ddl">>, PostVals, <<>>),
    DownDdl = proplists:get_value(<<"down_ddl">>, PostVals, <<>>),
    app_ddl_ds:save(AdmUserId, Type, NewVsn, OldVsn, Status, Ddl, DownDdl),
    imboy_response:success(Req0, PostVals, "success.").

delete(<<"DELETE">>, Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Id = proplists:get_value(<<"id">>, PostVals, ""),

    Where = <<"status = 0 AND id = ", (ec_cnv:to_binary(Id))/binary>>,
    app_ddl_ds:delete(Where),
    imboy_response:success(Req0, PostVals, "success.").
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
