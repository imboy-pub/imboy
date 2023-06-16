-module(collect_logic).
%%%
% collect 业务逻辑模块
% collect business logic module
%%%

-export ([page/4]).
-export ([add/6]).
-export ([remove/2]).
-export ([change/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% 用户的收藏分页列表
-spec page(integer(), integer(), binary(), binary()) -> list().
page(Page, Size, Where, OrderBy) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = collect_user_repo:count_for_where(Where),
    case collect_user_repo:page_for_where(Size, Offset, Where, OrderBy) of
        {ok, _, []} ->
            imboy_response:page_payload(Total, Page, Size, []);
        {ok, ColumnLi, Items0} ->
            Items1 = [tuple_to_list(Item) || Item <- Items0],
            Items2 = [lists:zipwith(
                fun(X, Y) -> {X, Y} end,
                ColumnLi, Row
                ) ||
                    Row <- Items1
            ],
            imboy_response:page_payload(Total, Page, Size, Items2);
        _ ->
            imboy_response:page_payload(Total, Page, Size, [])
    end.

-spec add(integer(), binary(), binary(), list(), binary(), binary()) ->
    {ok, binary()} | {error, binary()}.

% Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息
% 检查 Kind 类型
add(Uid, Kind, KindId, Info, Source, Remark) when is_integer(Kind) ->
    add(Uid, integer_to_binary(Kind), KindId, Info, Source, Remark);

add(_Uid, _Kind, _KindId, _Info, <<"">>, _Remark) ->
    {error, <<"source is empty">>};
add(_Uid, _Kind, <<"">>, _Info, _Source, _Remark) ->
    {error, <<"kind_id is empty">>};
add(_Uid, _Kind, _KindId, [], _Source, _Remark) ->
    {error, <<"kind info is empty">>};
% Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息
add(Uid, <<"6">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = collect_user_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/png">>,
    Info2 = get_info(Count, "c_location", MimeType, <<"thumb">>, Info),
    add_kind(Count, Uid2, <<"6">>, KindId, Info2, Source, Remark),
    {ok, <<"success">>};
add(Uid, <<"5">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = collect_user_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"octet-stream">>,
    Info2 = get_info(Count, "c_file", MimeType, <<"uri">>, Info),
    add_kind(Count, Uid2, <<"5">>, KindId, Info2, Source, Remark),
    {ok, <<"success">>};
add(Uid, <<"4">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Count = collect_user_repo:count_by_uid_kind_id(Uid, KindId),
    case Count of
        0 ->
            Payload = maps:from_list(proplists:get_value(<<"payload">>, Info)),
            % ?LOG([4, 'Payload', Payload]),
            Thumb= maps:from_list(maps:get(<<"thumb">>, Payload)),
            ThumbUri= maps:get(<<"uri">>, Thumb),
            Thumb2 = upload("c_video_thum", <<"image/jpeg">>, ThumbUri),
            Thumb3 = Thumb#{
                <<"md5">> => maps:get(<<"md5">>, Thumb2),
                <<"size">> => maps:get(<<"size">>, Thumb2),
                <<"uri">> => maps:get(<<"url">>, Thumb2)
            },

            Video= maps:from_list(maps:get(<<"video">>, Payload)),
            VideoUri= maps:get(<<"uri">>, Video),
            Video2 = upload("c_video", <<"octet-stream">>, VideoUri),
            Video3 = Video#{
                <<"md5">> => maps:get(<<"md5">>, Video2),
                <<"size">> => maps:get(<<"size">>, Video2),
                <<"uri">> => maps:get(<<"url">>, Video2)
            },

            Payload2 = Payload#{
                <<"thumb">> => Thumb3,
                <<"video">> => Video3
            },
            % Uri = proplists:get_value(<<"uri">>, Info),
            Info2 = maps:from_list(Info),
            Info3 = Info2#{
                <<"payload">> => Payload2
            },

            Uid2 = integer_to_binary(Uid),
            % ?LOG(['k4', Count, Info3]),
            Info4 =jsone:encode(Info3, [native_forward_slash]),
            add_kind(Count, Uid2, <<"4">>, KindId, Info4, Source, Remark);
        _ ->
            ok
    end,
    {ok, <<"success">>};

add(Uid, <<"3">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = collect_user_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"audio/aac">>,
    Info2 = get_info(Count, "c_audio", MimeType, <<"uri">>, Info),
    add_kind(Count, Uid2, <<"3">>, KindId, Info2, Source, Remark),
    {ok, <<"success">>};
add(Uid, <<"2">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = collect_user_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/jpeg">>,
    Info2 = get_info(Count, "c_img", MimeType, <<"uri">>, Info),
    add_kind(Count, Uid2, <<"2">>, KindId, Info2, Source, Remark),
    {ok, <<"success">>};

add(Uid, <<"1">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Count = collect_user_repo:count_by_uid_kind_id(Uid, KindId),
    Uid2 = integer_to_binary(Uid),
    Info2 =jsone:encode(Info, [native_forward_slash]),
    add_kind(Count, Uid2, <<"1">>, KindId, Info2, Source, Remark),
    {ok, <<"success">>};
add(_Uid, _Kind, _KindId, _Info, _Source, _Remark) ->
    {error, <<"Unsupported collection kind">>}.

remove(Uid, KindId) ->
    collect_user_repo:delete(Uid, KindId).

change(Uid, [{<<"kind_id">>, KindId}]) ->
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % collect_user_repo:update(Uid, KindId);
    NowTs = imboy_dt:millisecond(),
    collect_user_repo:update(Uid, KindId, [
        {<<"updated_at">>, integer_to_binary(NowTs)}
    ]),
    ok;
change(_Uid, PostVals) ->
    KindId = proplists:get_value(<<"kind_id">>, PostVals, ""),
    lager:info("change KindId ~p; post ~p~n", [KindId, PostVals]),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % collect_user_repo:update(Uid, KindId);
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

get_info(0, Prefix, MimeType, Key, Info) ->
    Payload = maps:from_list(proplists:get_value(<<"payload">>, Info)),

    {ok, Uri} = maps:find(Key, Payload),
    M1 = upload(Prefix, MimeType, Uri),

    Payload2 = Payload#{
        <<"md5">> => maps:get(<<"md5">>, M1),
        <<"size">> => maps:get(<<"size">>, M1),
        Key => maps:get(<<"url">>, M1)
    },
    % Uri = proplists:get_value(<<"uri">>, Info),
    Info2 = maps:from_list(Info),
    Info3 = Info2#{
        <<"payload">> => Payload2
    },
    jsone:encode(Info3, [native_forward_slash]);
get_info(_Count, _Prefix, _MimeType, _Key, _Info) ->
    % 已经收藏，不需要再上传附件了
    ok.

upload(Prefix, MimeType, Uri) ->
    Uri2 = imboy_uri:check_auth(Uri),
    {UrlMap, UpData} = imboy_uri:get_params(binary_to_list(Uri2)),

    FileName = list_to_binary(filename:basename(maps:get(path, UrlMap))),
    % ?LOG(['Uri2', Uri2]),
    FilePath = <<"./", FileName/binary>>,

    imboy_uri:download(Uri2, FilePath),

    BaseUrl = imboy_func:env(upload_for_collect),

    TS = {_, _, _Micro} = os:timestamp(),
    {{Year, Month, Day}, {Hour, _M, _S}} = calendar:now_to_universal_time(TS),
    % "/$prefix/${dt.year}${dt.month}/${dt.day}_${dt.hour}/"
    Path = io_lib:format(
        "/~s/~p~p/~p_~p/",
        [Prefix, Year, Month, Day, Hour]
    ),

    % ?LOG(['UpData', UpData]),
    UpData2 = UpData#{
        "output" => "json2"
        % , "path" => "/collect/image/"
        , "path" => Path
        , "scene" => maps:get("s", UpData, "")
        , "filename" => FileName
    },
    % ?LOG(['UpData2', UpData2]),
    UpUrl = <<BaseUrl/binary, "/upload">>,

    FieldName = <<"file">>,
    {ok, Resp} = imboy_uri:upload(
        UpUrl
        , FilePath
        , FieldName
        , MimeType
        , [{list_to_binary(K), list_to_binary(V)} || {K, V} <- maps:to_list(UpData2), is_list(K), is_list(V)]
    ),
    % 删除成功后删除本地数据
    file:delete(FilePath),

    % ?LOG([upload_Resp, UpUrl, Resp]),
    #{
        <<"md5">> => maps:get(<<"md5">>, maps:get(<<"data">>, Resp)),
        <<"size">> => maps:get(<<"size">>, maps:get(<<"data">>, Resp)),
        <<"url">> => maps:get(<<"url">>, maps:get(<<"data">>, Resp))
    }.

-spec add_kind(integer(), binary(), binary(), binary(), binary(), binary(), binary()) ->
    ok.
add_kind(0, Uid, Kind, KindId, Info, Source, Remark) ->
    NowTs = imboy_dt:millisecond(),
    imboy_db:with_transaction(fun(Conn) ->
        CreatedAt = integer_to_binary(NowTs),
        Column1 = <<"(kind, kind_id, info, referer_time, creator_user_id, created_at)">>,
        Tb1 = <<"public.collect_resource">>,

        UpSql1 = <<" UPDATE SET updated_at = ", CreatedAt/binary, ", referer_time = public.collect_resource.referer_time + 1;">>,

        Sql1 = <<"INSERT INTO ", Tb1/binary," ",
               Column1/binary, " VALUES(",
               Kind/binary, ", '",
               KindId/binary, "', '",
               Info/binary, "'::text, ",
               "1, ",
               Uid/binary, ", ",
               CreatedAt/binary, ") ON CONFLICT (kind_id) DO ", UpSql1/binary>>,
        % ?LOG([sql1, Sql1]),
        {ok, Stmt1} = epgsql:parse(Conn, Sql1),
        epgsql:execute_batch(Conn, [{Stmt1, []}]),

        UpSql2 = <<" UPDATE SET updated_at = ", CreatedAt/binary, ", status = 1;">>,
        Tb2 = <<"public.collect_user">>,
        Column2 = <<"(user_id, kind, kind_id, source, remark, created_at)">>,
        Sql2 = <<"INSERT INTO ", Tb2/binary," ",
               Column2/binary, " VALUES(",
               Uid/binary, ", ",
               Kind/binary, ", '",
               KindId/binary, "', '",
               Source/binary, "', '",
               Remark/binary, "', ",
               CreatedAt/binary, ") ON CONFLICT (user_id, status, kind_id) DO ", UpSql2/binary>>,
        % ?LOG([sql2, Sql2]),
        {ok, Stmt2} = epgsql:parse(Conn, Sql2),

        epgsql:execute_batch(Conn, [{Stmt2, []}]),
        ok
    end),
    ok;
add_kind(_Count, _Kind, _Uid, _KindId, _Info, _Source, _Remark) ->
    % 已收藏
    ok.

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
