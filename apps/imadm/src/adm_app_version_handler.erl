-module(adm_app_version_handler).
%%%
% adm_app_version 控制器模块
% adm_app_version controller module
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
    % Where = imboy_cnv:implode("", [<<"user_id=">>, CurrentUid]),
    % Where2 = <<"status > 0 AND ", Where/binary>>,
    Where = <<"1=1">>,
    Column = <<"*">>,
    Tb = app_version_repo:tablename(),
    OrderBy = <<"sort desc, updated_at desc">>,
    Payload = imboy_db:page(Page, Size, Tb, Where, OrderBy, Column),
    imboy_response:success(Req0, Payload);
index(<<"GET">>, _, Req0, State) ->
    {ok, Body} = imboy_dtl:template(app_version_index_dtl, [
        {attach_token, ""}
    ] ++ imboy_dtl:imadm_param(State), imadm),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_index_dtl.html"])),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).


save(<<"POST">>, Req0, _State) ->
    % AdmUserId = maps:get(adm_user_id, State),

    PostVals = imboy_req:post_params(Req0),

    RCode = proplists:get_value(<<"region_code">>, PostVals, <<"cn">>),
    Type = proplists:get_value(<<"type">>, PostVals, <<>>),
    PkgName = proplists:get_value(<<"package_name">>, PostVals, <<>>),
    AppName = proplists:get_value(<<"app_name">>, PostVals, <<>>),
    Vsn = proplists:get_value(<<"vsn">>, PostVals, <<>>),
    SKey = proplists:get_value(<<"sign_key">>, PostVals, <<>>),
    DUrl = proplists:get_value(<<"download_url">>, PostVals, <<>>),
    Desc = proplists:get_value(<<"description">>, PostVals, <<>>),
    ForceUpdate = proplists:get_value(<<"force_update">>, PostVals, 2),
    Status = proplists:get_value(<<"status">>, PostVals, 0),
    Id = proplists:get_value(<<"id">>, PostVals, 0),
    Data = #{
        id => Id
        , region_code => RCode
        , type => Type
        , package_name => PkgName
        , app_name => AppName
        , vsn => Vsn
        , sort => adm_app_version_logic:vsn_sort(Vsn)
        , sign_key => SKey
        , download_url => DUrl
        , description => Desc
        , force_update => ec_cnv:to_integer(ForceUpdate)
        , status => ec_cnv:to_integer(Status)
    },
    adm_app_version_logic:save(Data),
    imboy_response:success(Req0, PostVals, "success.").

delete(<<"DELETE">>, Req0, _State) ->
    PostVals = imboy_req:post_params(Req0),
    Id = proplists:get_value(<<"id">>, PostVals, ""),

    Where = <<"status = 0 AND id = ", (ec_cnv:to_binary(Id))/binary>>,
    adm_app_version_logic:delete(Where),
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
