-module(user_collect_logic).

%%%
% user_collect 业务逻辑模块
% user collect business logic module
%%%

-export([add/6]).
-export([remove/2]).
-export([change/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


-spec add(integer(), binary(), binary(), map(), binary(), binary()) -> {ok, binary()} | {error, binary()}.

% Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片
% 检查 Kind 类型
add(Uid, Kind, KindId, Info, Source, Remark) when is_integer(Kind) ->
    add(Uid, integer_to_binary(Kind), KindId, Info, Source, Remark);

add(_Uid, _Kind, _KindId, _Info, <<"">>, _Remark) ->
    {error, <<"source is empty">>};
add(_Uid, _Kind, <<"">>, _Info, _Source, _Remark) ->
    {error, <<"kind_id is empty">>};
add(_Uid, _Kind, _KindId, Info, _Source, _Remark) when Info =:= [] orelse Info =:= #{} ->
    {error, <<"kind info is empty">>};
add(_Uid, _Kind, _KindId, Info, _Source, _Remark) when not is_map(Info) ->
    {error, <<"kind info must be map">>};
% Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片
add(Uid, <<"7">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    Info2 = jsone:encode(Info, [native_forward_slash]),
    add_kind(Count, Uid, <<"7">>, KindId, Info2, Source, Remark, []),
    {ok, <<"success">>};
add(Uid, <<"6">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/png">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"thumb">>, Info),
    add_kind(Count, Uid, <<"6">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"5">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"octet-stream">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
    add_kind(Count, Uid, <<"5">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"4">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    case Count of
        0 ->
            Payload = ensure_map(maps:get(<<"payload">>, Info, #{})),
            % ?DEBUG_LOG([4, 'Payload', Payload]),
            Thumb = ensure_map(maps:get(<<"thumb">>, Payload, #{})),
            ThumbUri = maps:get(<<"uri">>, Thumb),
            {ThumbMap, _} = imboy_uri:get_params(ThumbUri),
            ThumbPath = maps:get(path, ThumbMap),

            Video = ensure_map(maps:get(<<"video">>, Payload, #{})),
            VideoUri = maps:get(<<"uri">>, Video),
            {VideoMap, _} = imboy_uri:get_params(VideoUri),
            VideoPath = maps:get(path, VideoMap),

            Attach1 = #{
                        <<"md5">> => maps:get(<<"md5">>, Thumb),
                        <<"mime_type">> => <<"image/jpeg">>,
                        <<"name">> => maps:get(<<"name">>, Thumb, <<>>),
                        <<"path">> => ThumbPath,
                        <<"url">> => ThumbUri,
                        <<"size">> => maps:get(<<"size">>, Thumb, 0)
                       },
            Attach2 = #{
                        <<"md5">> => maps:get(<<"md5">>, Video),
                        <<"mime_type">> => <<"octet-stream">>,
                        <<"name">> => maps:get(<<"name">>, Video, <<>>),
                        <<"path">> => VideoPath,
                        <<"url">> => VideoUri,
                        <<"size">> => maps:get(<<"size">>, Video, maps:get(<<"filesize">>, Video, 0))
                       },

            Info2 = jsone:encode(Info, [native_forward_slash]),
            % ?DEBUG_LOG(['k4', Count, Info2]),
            add_kind(Count, Uid, <<"4">>, KindId, Info2, Source, Remark, [Attach1, Attach2]);
        _ ->
            ok
    end,
    {ok, <<"success">>};
add(Uid, <<"3">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"audio/aac">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
    add_kind(Count, Uid, <<"3">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"2">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/jpeg">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
    ok = imboy_log:info(io_lib:format("user_collect_logic/add_2: Count ~p, ~n", [[Count, Uid, KindId, Source, Remark]])),
    ok = imboy_log:info(io_lib:format("user_collect_logic/add_2: Count ~p, Attach ~p ~n", [Count, Attach])),
    % imboy_log:info(io_lib:format("user_collect_logic/add_2: Count ~p, Info2 ~p ~n", [Count, Info2])),
    add_kind(Count, Uid, <<"2">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};

add(Uid, <<"1">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    Info2 = jsone:encode(Info, [native_forward_slash]),
    add_kind(Count, Uid, <<"1">>, KindId, Info2, Source, Remark, []),
    {ok, <<"success">>};
add(_Uid, _Kind, _KindId, _Info, _Source, _Remark) ->
    {error, <<"Unsupported collection kind">>}.


remove(Uid, KindId) ->
    user_collect_repo:delete(Uid, KindId).


% 转发收藏消息回调，更新updated_at的值
change(Uid, <<"transpond_callback">>, KindId, _PostVals) ->
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % user_collect_repo:update(Uid, KindId);
    NowTs = imboy_dt:now(),
    _ = user_collect_repo:update(Uid, KindId, #{<<"updated_at">> => NowTs}),
    ok;
change(Uid, <<"remark">>, KindId, PostVals) ->
    Remark = maps:get(<<"remark">>, PostVals, ""),
    % user_collect_repo:update(Uid, KindId);
    Data = #{
        <<"updated_at">> => imboy_dt:now(),
        <<"remark">> => Remark
    },
    _ = user_collect_repo:update(Uid, KindId, Data),
    ok;
change(_Uid, _Action, _KindId, _PostVals) ->
    % KindId = proplists:get_value(<<"kind_id">>, PostVals, ""),
    % imboy_log:info("change KindId ~p; post ~p~n", [KindId, PostVals]),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % user_collect_repo:update(Uid, KindId);
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(_) ->
    #{}.

get_info(0, MimeType, Key, Info) ->
    PayloadIn = maps:get(<<"payload">>, Info, #{}),
    Payload0 = if
        is_map(PayloadIn) -> PayloadIn;
        true -> PayloadIn
    end,
    Payload =
        if
            is_binary(Payload0) ->
                jsone:decode(Payload0, [{object_format, map}]);
            true ->
                Payload0
        end,
    {ok, Uri} = maps:find(Key, Payload),
    Md5 = maps:get(<<"md5">>, Payload),
    Size = maps:get(<<"size">>, Payload, 0),

    UriList = case Uri of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L) -> L;
        _ -> ""
    end,
    {UrlMap, _UpData} = imboy_uri:get_params(UriList),
    Path = maps:get(path, UrlMap),

    % Uri = proplists:get_value(<<"uri">>, Info),
    Attach = #{
               <<"md5">> => Md5,
               <<"mime_type">> => MimeType,
               <<"name">> => maps:get(<<"name">>, Payload, <<>>),
               <<"path">> => Path,
               <<"url">> => Uri,
               <<"size">> => Size
              },
    {Attach, jsone:encode(Info, [native_forward_slash])};
get_info(_Count, _MimeType, _Key, _Info) ->
    % 已经收藏，不需要再上传附件了
    {#{}, ok}.


-spec add_kind(integer(), integer(), binary(), binary(), binary(), binary(), binary(), list()) -> ok.
% add_kind(_, _, _, _KindId, ok, _Source, _Remark, _Attach)
%     ok;
add_kind(0, Uid, KindBin, KindId, Info, Source, Remark, Attach) ->
    NowTs = imboy_dt:now(),
    ok = imboy_log:info(io_lib:format("user_collect_logic/add_kind/8: NowTs ~p ~n", [NowTs])),
    Kind = binary_to_integer(KindBin),
    imboy_pg:with_tx(fun(Conn) ->
        attachment_repo:save(Conn, NowTs, Uid, Attach),
        AttachMd5 =
            case [maps:get(<<"md5">>, Item) || Item <- Attach] of
                [] -> <<>>;
                Md5List ->
                    % 使用imboy_cnv工具函数替代手工处理
                    imboy_cnv:implode(<<",">>, Md5List)
            end,
        % 构建数据映射
        Data = #{
            <<"user_id">> => Uid,
            <<"kind">> => Kind,
            <<"kind_id">> => KindId,
            <<"source">> => Source,
            <<"remark">> => Remark,
            <<"info">> => imboy_hasher:encoded_val(Info),
            <<"attach_md5">> => AttachMd5,
            <<"created_at">> => NowTs,
            <<"updated_at">> => NowTs,
            <<"status">> => 1
        },

        % 构建带ON CONFLICT的INSERT SQL
        {Sql, Params} = imboy_pg_sql:insert(<<"public.user_collect">>, Data, <<>>),
        OnConflictUpdate = <<
            "ON CONFLICT (user_id, status, kind_id) DO UPDATE SET "
            "updated_at = EXCLUDED.updated_at, "
            "status = 1"
        >>,
        FullSql = [Sql, <<" ">>, OnConflictUpdate],
        _ = imboy_pg:execute(Conn, FullSql, Params),
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
