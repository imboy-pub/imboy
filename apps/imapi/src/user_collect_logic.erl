-module(user_collect_logic).
%%%
% user_collect 业务逻辑模块
% user collect business logic module
%%%

-export([page/4]).
-export([add/6]).
-export([remove/2]).
-export([change/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%%% 用户的收藏分页列表
-spec page(integer(), integer(), binary(), binary()) -> list().
page(Page, Size, Where, OrderBy) when Page > 0 ->
    Info = imboy_hasher:decoded_field(<<"info">>),
    Column = <<"kind, kind_id, source, created_at, updated_at, tag, ", Info/binary>>,
    Tb = user_collect_repo:tablename(),
    imboy_db:page(Page, Size, Tb, Where, OrderBy, Column).


-spec add(integer(), binary(), binary(), list(), binary(), binary()) -> {ok, binary()} | {error, binary()}.

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
    Info2 = jsone:encode(Info, [native_forward_slash]),
    add_kind(Count, Uid2, <<"7">>, KindId, Info2, Source, Remark, []),
    {ok, <<"success">>};
add(Uid, <<"6">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/png">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"thumb">>, Info),
    add_kind(Count, Uid2, <<"6">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"5">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"octet-stream">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
    add_kind(Count, Uid2, <<"5">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"4">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    case Count of
        0 ->
            Payload = maps:from_list(proplists:get_value(<<"payload">>, Info)),
            % ?LOG([4, 'Payload', Payload]),
            Thumb = maps:from_list(maps:get(<<"thumb">>, Payload)),
            ThumbUri = maps:get(<<"uri">>, Thumb),
            {ThumbMap, _} = imboy_uri:get_params(ThumbUri),
            ThumbPath = maps:get(path, ThumbMap),

            Video = maps:from_list(maps:get(<<"video">>, Payload)),
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

            Uid2 = integer_to_binary(Uid),
            Info2 = jsone:encode(Info, [native_forward_slash]),
            % ?LOG(['k4', Count, Info2]),
            add_kind(Count, Uid2, <<"4">>, KindId, Info2, Source, Remark, [Attach1, Attach2]);
        _ ->
            ok
    end,
    {ok, <<"success">>};
add(Uid, <<"3">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"audio/aac">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
    add_kind(Count, Uid2, <<"3">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};
add(Uid, <<"2">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Uid2 = integer_to_binary(Uid),
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/jpeg">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
    imboy_log:info(io_lib:format("user_collect_logic/add_2: Count ~p, ~n", [[Count, Uid2, KindId, Source, Remark]])),
    imboy_log:info(io_lib:format("user_collect_logic/add_2: Count ~p, Attach ~p ~n", [Count, Attach])),
    % imboy_log:info(io_lib:format("user_collect_logic/add_2: Count ~p, Info2 ~p ~n", [Count, Info2])),
    add_kind(Count, Uid2, <<"2">>, KindId, Info2, Source, Remark, [Attach]),
    {ok, <<"success">>};

add(Uid, <<"1">>, KindId, Info, Source, Remark) when is_list(Info) ->
    Count = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
    Uid2 = integer_to_binary(Uid),
    Info2 = jsone:encode(Info, [native_forward_slash]),
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
    NowTs = imboy_dt:now(),
    user_collect_repo:update(Uid, KindId, [{<<"updated_at">>, NowTs}]),
    ok;
change(Uid, <<"remark">>, KindId, PostVals) ->
    Remark = proplists:get_value(<<"remark">>, PostVals, ""),
    % user_collect_repo:update(Uid, KindId);
    NowTs = imboy_dt:now(),
    user_collect_repo:update(Uid,
                             KindId,
                             [{<<"updated_at">>, NowTs},
                              {<<"remark">>, Remark}]),
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


get_info(0, MimeType, Key, Info) ->
    Payload0 = maps:from_list(proplists:get_value(<<"payload">>, Info)),
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

    {UrlMap, _UpData} = imboy_uri:get_params(binary_to_list(Uri)),
    Path = maps:get(path, UrlMap),

    % Uri = proplists:get_value(<<"uri">>, Info),
    Info2 = maps:from_list(Info),
    Attach = #{
               <<"md5">> => Md5,
               <<"mime_type">> => MimeType,
               <<"name">> => maps:get(<<"name">>, Payload, <<>>),
               <<"path">> => Path,
               <<"url">> => Uri,
               <<"size">> => Size
              },
    {Attach, jsone:encode(Info2, [native_forward_slash])};
get_info(_Count, _MimeType, _Key, _Info) ->
    % 已经收藏，不需要再上传附件了
    {#{}, ok}.


-spec add_kind(integer(), binary(), binary(), binary(), binary(), binary(), binary(), list()) -> ok.
% add_kind(_, _, _, _KindId, ok, _Source, _Remark, _Attach)
%     ok;
add_kind(0, Uid, Kind, KindId, Info, Source, Remark, Attach) ->
    NowTs = imboy_dt:now(),
    imboy_log:info(io_lib:format("user_collect_logic/add_kind/8: NowTs ~p ~n", [NowTs])),
    imboy_db:with_transaction(fun(Conn) ->
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
            <<"info">> => {raw, imboy_hasher:encoded_val(Info)}, % 原生JSON处理
            <<"attach_md5">> => AttachMd5,
            <<"created_at">> => NowTs, % 使用原生时间格式
            <<"updated_at">> => NowTs,     % 冲突更新用时间
            <<"status">> => 1                     % 显式设置初始状态
        },

        % 构建ON CONFLICT子句（使用EXCLUDED优化）
        OnConflictUpdate = <<
            "ON CONFLICT (user_id, status, kind_id) DO UPDATE SET "
            "updated_at = EXCLUDED.updated_at, "  % 直接使用插入值
            "status = 1"                          % 硬编码状态更新
        >>,

        % 单行调用实现UPSERT
        imboy_db:add(Conn, <<"public.user_collect">>, Data, OnConflictUpdate),
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
