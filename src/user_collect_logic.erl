-module(user_collect_logic).
%%%
% user_collect 业务逻辑模块
% user collect business logic module
%%%

-export ([page/4]).
-export ([add/6]).
-export ([remove/2]).
-export ([change/4]).

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
    Total = user_collect_repo:count_for_where(Where),
    case user_collect_repo:page_for_where(Size, Offset, Where, OrderBy) of
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

% Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片
% 检查 Kind 类型
add(Uid, Kind, KindId, Info, Source, Remark) when is_integer(Kind) ->
    add(Uid, integer_to_binary(Kind), KindId, Info, Source, Remark);

add(_Uid, _Kind, _KindId, _Info, <<"">>, _Remark) ->
    {error, <<"source is empty">>};
add(_Uid, _Kind, <<"">>, _Info, _Source, _Remark) ->
    {error, <<"kind_id is empty">>};
add(_Uid, _Kind, _KindId, [], _Source, _Remark) ->
    {error, <<"kind info is empty">>};
% Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片
add(Uid, <<"7">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    Uid2 = integer_to_binary(Uid),
    Info2 =jsone:encode(Info, [native_forward_slash]),
    add_kind(Count, Uid2, <<"7">>, KindId, Info2, Source, Remark, []),
    {ok, <<"success">>};
add(Uid, <<"6">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/png">>,
    {Attach, Info2} = get_info(Count, "c_location", MimeType, <<"thumb">>, Info),
    add_kind(Count, Uid2, <<"6">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"5">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"octet-stream">>,
    {Attach, Info2} = get_info(Count, "c_file", MimeType, <<"uri">>, Info),
    add_kind(Count, Uid2, <<"5">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"4">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    case Count of
        0 ->
            Payload = maps:from_list(proplists:get_value(<<"payload">>, Info)),
            % ?LOG([4, 'Payload', Payload]),
            Thumb= maps:from_list(maps:get(<<"thumb">>, Payload)),
            ThumbUri= maps:get(<<"uri">>, Thumb),
            MimeType = <<"image/jpeg">>,
            Thumb2 = upload("c_video_thumb", MimeType, ThumbUri),
            Thumb3 = Thumb#{
                <<"md5">> => maps:get(<<"md5">>, Thumb2),
                <<"size">> => maps:get(<<"size">>, Thumb2),
                <<"uri">> => maps:get(<<"url">>, Thumb2)
            },

            VideoMimeType = <<"octet-stream">>,
            Video= maps:from_list(maps:get(<<"video">>, Payload)),
            VideoUri= maps:get(<<"uri">>, Video),
            Video2 = upload("c_video", VideoMimeType, VideoUri),
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

            Attach1 = #{
                <<"md5">> => maps:get(<<"md5">>, Thumb2),
                <<"mime_type">> => MimeType,
                <<"name">> => <<"">>,
                <<"path">> => maps:get(<<"path">>, Thumb2),
                <<"url">> => maps:get(<<"url">>, Thumb2),
                <<"size">> => maps:get(<<"size">>, Thumb2)
            },
            Attach2 = #{
                <<"md5">> => maps:get(<<"md5">>, Video2),
                <<"mime_type">> => VideoMimeType,
                <<"name">> => <<"">>,
                <<"path">> => maps:get(<<"path">>, Video2),
                <<"url">> => maps:get(<<"url">>, Video2),
                <<"size">> => maps:get(<<"size">>, Video2)
            },

            Uid2 = integer_to_binary(Uid),
            % ?LOG(['k4', Count, Info3]),
            Info4 =jsone:encode(Info3, [native_forward_slash]),
            add_kind(Count, Uid2, <<"4">>, KindId, Info4, Source, Remark, [Attach1, Attach2]);
        _ ->
            ok
    end,
    {ok, <<"success">>};
add(Uid, <<"3">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"audio/aac">>,
    {Attach, Info2} = get_info(Count, "c_audio", MimeType, <<"uri">>, Info),
    add_kind(Count, Uid2, <<"3">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"2">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/jpeg">>,
    {Attach, Info2} = get_info(Count, "c_img", MimeType, <<"uri">>, Info),
    lager:info(io_lib:format("user_collect_logic/add_2: Attach ~p ~n", [Attach])),
    add_kind(Count, Uid2, <<"2">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};

add(Uid, <<"1">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    Uid2 = integer_to_binary(Uid),
    Info2 =jsone:encode(Info, [native_forward_slash]),
    add_kind(Count, Uid2, <<"1">>, KindId, Info2, Source, Remark, []),
    {ok, <<"success">>};
add(_Uid, _Kind, _KindId, _Info, _Source, _Remark) ->
    {error, <<"Unsupported collection kind">>}.

remove(Uid, KindId) ->
    user_collect_repo:delete(Uid, KindId).

% 转发收藏消息回调，更新updated_at的值
change(Uid, <<"transpond_callback">>, KindId, _PostVals) ->
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % user_collect_repo:update(Uid, KindId);
    NowTs = imboy_dt:millisecond(),
    user_collect_repo:update(Uid, KindId, [
        {<<"updated_at">>, integer_to_binary(NowTs)}
    ]),
    ok;
change(_Uid, _Action, _KindId, _PostVals) ->
    % KindId = proplists:get_value(<<"kind_id">>, PostVals, ""),
    % lager:info("change KindId ~p; post ~p~n", [KindId, PostVals]),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % user_collect_repo:update(Uid, KindId);
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

get_info(0, Prefix, MimeType, Key, Info) ->
    Payload0 = maps:from_list(proplists:get_value(<<"payload">>, Info)),
    Payload = if
        is_binary(Payload0) ->
            jsone:decode(Payload0, [{object_format, map}]);
        true ->
            Payload0
    end,
    {ok, Uri} = maps:find(Key, Payload),
    M1 = upload(Prefix, MimeType, Uri),
    Md5 = maps:get(<<"md5">>, M1),
    Size = maps:get(<<"size">>, M1),
    Url = maps:get(<<"url">>, M1),
    Payload2 = Payload#{
        <<"md5">> => Md5,
        <<"size">> => Size,
        Key => Url
    },
    % Uri = proplists:get_value(<<"uri">>, Info),
    Info2 = maps:from_list(Info),
    Info3 = Info2#{
        <<"payload">> => Payload2
    },
    Attach = #{
        <<"md5">> => Md5,
        <<"mime_type">> => MimeType,
        <<"name">> => maps:get(<<"name">>, Payload, <<>>),
        <<"path">> => maps:get(<<"path">>, M1),
        <<"url">> => Url,
        <<"size">> => Size
    },
    {Attach, jsone:encode(Info3, [native_forward_slash])};
get_info(_Count, _Prefix, _MimeType, _Key, _Info) ->
    % 已经收藏，不需要再上传附件了
    {#{}, ok}.

upload(Prefix, MimeType, Uri) ->
    Uri2 = imboy_uri:check_auth(Uri),
    {UrlMap, UpData} = imboy_uri:get_params(binary_to_list(Uri2)),

    FileName = list_to_binary(filename:basename(maps:get(path, UrlMap))),
    % ?LOG(['Uri2', Uri2]),
    FilePath = <<"./", FileName/binary>>,

    imboy_uri:download(Uri2, FilePath),

    BaseUrl = config_ds:env(upload_for_collect),

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

    lager:info(io_lib:format("user_collect_logic/upload: UpUrl, ~p; Resp ~p ~n", [UpUrl, jsone:encode(Resp)])),
    Data = maps:get(<<"data">>, Resp),
    #{
        <<"md5">> => maps:get(<<"md5">>, Data),
        <<"size">> => maps:get(<<"size">>, Data),
        <<"path">> => maps:get(<<"path">>, Data),
        <<"url">> => maps:get(<<"url">>, Data)
    }.

-spec add_kind(integer(), binary(), binary(), binary(), binary(), binary(), binary(), list()) ->
    ok.
% add_kind(_, _, _, _KindId, ok, _Source, _Remark, _Attach)
%     ok;
add_kind(0, Uid, Kind, KindId, Info, Source, Remark, Attach) ->
    NowTs = imboy_dt:millisecond(),
    % logger:error("user_collect_logic:add_kind ~p~n", [NowTs]),
    imboy_db:with_transaction(fun(Conn) ->
        CreatedAt = integer_to_binary(NowTs),
        attachment_repo:save(Conn, CreatedAt, Uid, Attach),

        Md5 = erlang:iolist_to_binary([
            [",",maps:get(<<"md5">>, Item)] || Item <- Attach
        ]),
        AttachMd5 = case Md5 of
            <<>> ->
                <<>>;
            <<",", Md52/binary>> ->
                Md52
        end,
        Info2 = imboy_hasher:encoded_val(Info),
        UpSql2 = <<" UPDATE SET updated_at = ", CreatedAt/binary, ", status = 1;">>,
        Tb2 = <<"public.user_collect">>,
        Column2 = <<"(user_id, kind, kind_id, source, remark, info, attach_md5, created_at)">>,
        Sql2 = <<"INSERT INTO ", Tb2/binary," ",
               Column2/binary, " VALUES(",
               Uid/binary, ", ",
               Kind/binary, ", '",
               KindId/binary, "', '",
               Source/binary, "', '",
               Remark/binary, "', ",
               Info2/binary, ", '",
               AttachMd5/binary, "', ",
               CreatedAt/binary, ") ON CONFLICT (user_id, status, kind_id) DO ", UpSql2/binary>>,
        logger:error("user_collect_logic:add_kind ~s~n", [Sql2]),
        {ok, Stmt2} = epgsql:parse(Conn, Sql2),

        epgsql:execute_batch(Conn, [{Stmt2, []}]),
        ok
    end),
    ok;
add_kind(_Count, _Kind, _Uid, _KindId, _Info, _Source, _Remark, _) ->
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
