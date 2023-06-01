-module(collect_logic).
%%%
% collect 业务逻辑模块
% collect business logic module
%%%

-export ([page/3]).
-export ([add/5]).

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
-spec page(integer(), integer(), integer()) -> list().
page(Uid, Page,  Size) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = collect_user_repo:count_for_uid(Uid),
    case collect_user_repo:page_for_uid(Uid, Size, Offset) of
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

-spec add(integer(), binary(), binary(), list(), binary()) ->
    {ok, binary()} | {error, binary()}.

% Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件
% 检查 Kind 类型
add(Uid, Kind, KindId, Info, Remark) when is_integer(Kind) ->
    add(Uid, integer_to_binary(Kind), KindId, Info, Remark);

add(_Uid, _Kind, <<"">>, _Info, _Remark) ->
    {error, <<"kind_id is empty">>};
add(_Uid, _Kind, _KindId, [], _Remark) ->
    {error, <<"kind info is empty">>};
% Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件
add(Uid, <<"2">>, KindId, Info, Remark) when is_list(Info) ->
    ?LOG([Uid, <<"2">>, KindId, Info, Remark]),
    % Count = collect_user_repo:count_by_uid_kind_id(Uid, KindId),
    % Uid2 = integer_to_binary(Uid),
    Payload = proplists:get_value(<<"payload">>, Info),
    FileName = proplists:get_value(<<"name">>, Payload),
    Uri = proplists:get_value(<<"uri">>, Payload),
    Width = proplists:get_value(<<"width">>, Payload),
    % ?LOG(['Uri', Uri]),
    % Uri2 = imboy_uri:exclusion_param(Uri, [<<"width">>]),
    Uri2 = imboy_uri:exclusion_param(Uri, [<<"width">>]),
    % ?LOG(['Uri2', Uri2]),
    FilePath = <<"./temp_", FileName/binary>>,
    % FilePath = <<"/Users/leeyi/workspace/imboy/imboy/", FileName/binary>>,

    Width2 = integer_to_binary(Width + 1),
    Uri3 = <<Uri2/binary, "&width=", Width2/binary>>,
    % ?LOG(['Uri3', Uri3]),
    imboy_uri:download(Uri3, FilePath),

    BaseUrl = imboy_func:env(upload_base_url),
    %  a s v
    UpData = imboy_req:get_params(binary_to_list(Uri2)),
    FileExt = filename:extension(FileName),
    % ?LOG(['UpData', UpData]),
    UpData2 = UpData#{
        "output" => "json2"
        % "/$prefix/${dt.year}${dt.month}/${dt.day}_${dt.hour}/"
        , "path" => "/collect/image/"
        , "scene" => maps:get("s", UpData, "")
        % , "md5" => "abcdefg"
        % , "filename" => binary_to_list(filename:basename(FilePath))
        , "filename" => binary_to_list(<<KindId/binary, FileExt/binary>>)
    },
    % ?LOG(['UpData2', UpData2]),
    UpUrl = <<BaseUrl/binary, "/upload">>,
    MimeType = <<"image/jpg">>,
    FieldName = <<"file">>,
    Resp = imboy_uri:upload(
        UpUrl
        , FilePath
        , FieldName
        , MimeType
        , [{list_to_binary(K), list_to_binary(V)} || {K, V} <- maps:to_list(UpData2), is_list(K), is_list(V)]
    ),
    ?LOG([upload_Resp, UpUrl, Resp]),
    % Uri = proplists:get_value(<<"uri">>, Info),
    % Info2 =jsone:encode(Info, [native_forward_slash]),
    % add_kind(Count, Uid2, <<"2">>, KindId, Info2, Remark),

    % 删除成功后删除本地数据
    file:delete(FilePath),
    {ok, <<"success">>};
add(Uid, <<"1">>, KindId, Info, Remark) when is_list(Info) ->
    Count = collect_user_repo:count_by_uid_kind_id(Uid, KindId),
    Uid2 = integer_to_binary(Uid),
    Info2 =jsone:encode(Info, [native_forward_slash]),
    add_kind(Count, Uid2, <<"1">>, KindId, Info2, Remark),
    {ok, <<"success">>};
add(_Uid, _Kind, _KindId, _Info, _Remark) ->
    {error, <<"Unsupported collection kind">>}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

-spec add_kind(integer(), binary(), binary(), binary(), binary(), binary()) ->
    ok.
add_kind(0, Uid, Kind, KindId, Info, Remark) ->
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
        Column2 = <<"(user_id, kind, kind_id, remark, created_at)">>,
        Sql2 = <<"INSERT INTO ", Tb2/binary," ",
               Column2/binary, " VALUES(",
               Uid/binary, ", ",
               "1, '",
               KindId/binary, "', '",
               Remark/binary, "', ",
               CreatedAt/binary, ") ON CONFLICT (user_id, status, kind_id) DO ", UpSql2/binary>>,
        % ?LOG([sql2, Sql2]),
        {ok, Stmt2} = epgsql:parse(Conn, Sql2),

        epgsql:execute_batch(Conn, [{Stmt2, []}]),
        ok
    end),
    ok;
add_kind(_Count, _Kind, _Uid, _KindId, _Info, _Remark) ->
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
