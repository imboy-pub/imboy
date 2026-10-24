-module(user_collect_logic).

-moduledoc "social_graph 收藏域稳定边界 —— HTTP 适配层经此模块访问。".
%% Stable social_graph collect domain boundary.

%%%
% user_collect 业务逻辑模块
% user collect business logic module
%%%

-export([add/6]).
-export([remove/2]).
-export([change/4]).
-export([page/5]).
-export([add_v1/6, update_v1/7, remove_dispatch/2, sealed_exists/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 添加收藏
%% 添加用户收藏内容，支持多种资源类型
%% @param Uid 用户ID
%% @param Kind 资源类型（1-7）
%% @param KindId 资源ID
%% @param Info 资源信息
%% @param Source 来源
%% @param Remark 备注
%% @return {ok, binary()} | {error, binary() | integer()}
-spec add(integer(), binary(), binary(), map() | list(), binary(), binary()) ->
    {ok, binary()} | {error, binary() | integer()}.

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
%% §9 legacy add 守卫（迁移 227 / PLAN 2026-10-24-e2ee-collect）：
%% sealed 行（任意 status，含墓碑）不可由 legacy 接口操作——status=1 时现状
%% ON CONFLICT 虽不覆盖密文，但墓碑行 INSERT 必撞跨 status 唯一约束（SQL 500）。
add(Uid, Kind, KindId, Info, Source, Remark) when
    is_binary(Kind), KindId =/= <<"">>, Source =/= <<"">>, is_map(Info)
->
    case user_collect_ds:sealed_exists(Uid, KindId) of
        true ->
            {error, ?ERR_COLLECT_SEALED_READONLY};
        false ->
            add_legacy(Uid, Kind, KindId, Info, Source, Remark)
    end.

-spec add_legacy(integer(), binary(), binary(), map(), binary(), binary()) ->
    {ok, binary()} | {error, binary()}.
add_legacy(Uid, <<"7">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_ds:count_by_uid_kind_id(Uid, KindId),
    Info2 = jsone:encode(Info, [native_forward_slash]),
    collect_result(add_kind(Count, Uid, <<"7">>, KindId, Info2, Source, Remark, []));
add_legacy(Uid, <<"6">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_ds:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"image/png">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"thumb">>, Info),
    collect_result(add_kind(Count, Uid, <<"6">>, KindId, Info2, Source, Remark, [Attach]));
add_legacy(Uid, <<"5">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_ds:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"octet-stream">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
    collect_result(add_kind(Count, Uid, <<"5">>, KindId, Info2, Source, Remark, [Attach]));
add_legacy(Uid, <<"4">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_ds:count_by_uid_kind_id(Uid, KindId),
    Result =
        case Count of
            0 ->
                Payload = ensure_map(maps:get(<<"payload">>, Info, #{})),
                % ?DEBUG_LOG([4, 'Payload', Payload]),
                Thumb = ensure_map(maps:get(<<"thumb">>, Payload, #{})),
                ThumbUri = maps:get(<<"uri">>, Thumb, <<>>),
                {ThumbMap, _} = elib_uri:get_params(ThumbUri),
                ThumbPath = maps:get(path, ThumbMap),

                Video = ensure_map(maps:get(<<"video">>, Payload, #{})),
                VideoUri = maps:get(<<"uri">>, Video, <<>>),
                {VideoMap, _} = elib_uri:get_params(VideoUri),
                VideoPath = maps:get(path, VideoMap),

                Attach1 = #{
                    %% 双读兼容：新消息 metadata 用 file_hash256，旧消息用 md5
                    <<"file_hash256">> => maps:get(
                        <<"file_hash256">>, Thumb, maps:get(<<"md5">>, Thumb, <<>>)
                    ),
                    <<"mime_type">> => <<"image/jpeg">>,
                    <<"name">> => maps:get(<<"name">>, Thumb, <<>>),
                    <<"path">> => ThumbPath,
                    <<"url">> => ThumbUri,
                    <<"size">> => maps:get(<<"size">>, Thumb, 0)
                },
                Attach2 = #{
                    <<"file_hash256">> => maps:get(
                        <<"file_hash256">>, Video, maps:get(<<"md5">>, Video, <<>>)
                    ),
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
    collect_result(Result);
add_legacy(Uid, <<"3">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_ds:count_by_uid_kind_id(Uid, KindId),
    MimeType = <<"audio/aac">>,
    {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
    collect_result(add_kind(Count, Uid, <<"3">>, KindId, Info2, Source, Remark, [Attach]));
add_legacy(Uid, <<"2">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_ds:count_by_uid_kind_id(Uid, KindId),
    case collect_image_attaches(Count, Info) of
        {multi, Attaches} ->
            Info2 = jsone:encode(Info, [native_forward_slash]),
            collect_result(
                add_kind(Count, Uid, <<"2">>, KindId, Info2, Source, Remark, Attaches)
            );
        single ->
            MimeType = <<"image/jpeg">>,
            {Attach, Info2} = get_info(Count, MimeType, <<"uri">>, Info),
            collect_result(
                add_kind(Count, Uid, <<"2">>, KindId, Info2, Source, Remark, [Attach])
            )
    end;
add_legacy(Uid, <<"1">>, KindId, Info, Source, Remark) when is_map(Info) ->
    Count = user_collect_ds:count_by_uid_kind_id(Uid, KindId),
    Info2 = jsone:encode(Info, [native_forward_slash]),
    collect_result(add_kind(Count, Uid, <<"1">>, KindId, Info2, Source, Remark, []));
add_legacy(_Uid, _Kind, _KindId, _Info, _Source, _Remark) ->
    {error, <<"Unsupported collection kind">>}.

collect_result(ok) ->
    {ok, <<"success">>};
collect_result({error, Reason}) ->
    ?ERROR_LOG({collect_persistence_failed, Reason}),
    {error, <<"收藏保存失败，请重试"/utf8>>};
collect_result(Other) ->
    ?ERROR_LOG({collect_persistence_unexpected_result, Other}),
    {error, <<"收藏保存失败，请重试"/utf8>>}.

%% @doc 删除收藏
%% 删除指定的收藏内容
%% @param Uid 用户ID
%% @param KindId 收藏ID
%% @return {ok, Count} | {error, Reason}
-spec remove(integer(), binary()) -> {ok, term()} | {error, term()}.
remove(Uid, KindId) ->
    remove_dispatch(Uid, KindId).

%% @doc 删除分流（§9，legacy remove 与 v1 remove 共用）：
%% sealed 行 → 墓碑化（服务端代执行，绝不物理 DELETE——防复活拦截被拆）；
%% legacy 行 / 无行 → 物理 DELETE（现状行为逐字节不变）。
-spec remove_dispatch(integer(), binary()) -> {ok, term()} | {error, term()}.
remove_dispatch(Uid, KindId) ->
    case user_collect_ds:find_any_by_uid_kind_id(Uid, KindId) of
        not_found ->
            {ok, deleted};
        {ok, Row} ->
            case maps:get(<<"key_version">>, Row, null) of
                null ->
                    user_collect_ds:delete(Uid, KindId);
                _Kv ->
                    user_collect_ds:tombstone_sealed(Uid, KindId)
            end
    end.

%% @doc 修改收藏属性
%% 修改收藏的备注等属性
%% @param Uid 用户ID
%% @param Action 操作类型
%% @param KindId 收藏ID
%% @param PostVals 参数映射
%% @return ok
-spec change(integer(), binary(), binary(), map()) -> ok | {error, integer()}.
change(Uid, <<"transpond_callback">>, KindId, _PostVals) ->
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % user_collect_ds:update(Uid, KindId);
    %% 仅 updated_at（最近使用排序），不触数据面/引用/版本——sealed 行放行
    NowTs = elib_dt:now(),
    _ = user_collect_ds:update(Uid, KindId, #{<<"updated_at">> => NowTs}),
    ok;
change(Uid, <<"remark">>, KindId, PostVals) ->
    %% §9：sealed 行的 source/remark/tag 为密文内字段（D1），legacy 明文列
    %% 写入既污染结构又绕过 generation 版本控制——拒绝。
    case user_collect_ds:sealed_exists(Uid, KindId) of
        true ->
            {error, ?ERR_COLLECT_SEALED_READONLY};
        false ->
            Remark = maps:get(<<"remark">>, PostVals, ""),
            Data = #{
                <<"updated_at">> => elib_dt:now(),
                <<"remark">> => Remark
            },
            _ = user_collect_ds:update(Uid, KindId, Data),
            ok
    end;
change(_Uid, _Action, _KindId, _PostVals) ->
    % KindId = proplists:get_value(<<"kind_id">>, PostVals, ""),
    % elib_log:info("change KindId ~p; post ~p~n", [KindId, PostVals]),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % user_collect_ds:update(Uid, KindId);
    ok.

%% @doc 分页查询用户收藏列表
%% 委托 DS 层执行带条件、排序和分页的收藏查询
%% @param Column 查询列
%% @param WhereMap 过滤条件
%% @param Order 排序字符串
%% @param Page 页码
%% @param Size 每页大小
%% @return {ok, map()} | {error, term()}
-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, WhereMap, Order, Page, Size) ->
    user_collect_ds:page(Column, WhereMap, Order, Page, Size).

%% ===================================================================
%% E2EE 收藏 v1（迁移 227 / PLAN 2026-10-24-e2ee-collect §7.2/§7.3）
%% 单行模型 + 记录修订号 generation + 用户级 key_version 高水位门。
%% info 列语义：key_version NULL = 服务端持钥密文（legacy）；
%% 非空 = 客户端 sealed base64（page 按此分流解码）。
%% ===================================================================

%% v1 参数上限：密文 256KiB（收藏 payload 含 remark/tag/引用声明足够）；
%% 引用 10 个（9 图+单视频/文件口径）；kind_id 沿用列宽 varchar(40)。
-define(V1_INFO_SEALED_MAX, 262144).
-define(V1_ATTACH_MAX, 10).
-define(V1_KIND_ID_MAX, 40).

%% @doc v1 创建/重新收藏（含墓碑复活路径，复用原行 CAS，不 INSERT）。
%% 全生命周期：key_state 门 → 附件存在性+授权 → 无行 INSERT(generation=1) /
%% 墓碑复活 UPDATE / 已收藏幂等判定。
-spec add_v1(integer(), integer(), binary(), binary(), integer(), [binary()]) ->
    {ok, map()} | {error, integer()}.
add_v1(Uid, Kind, KindId, InfoSealed, KeyVersion, AttachHashes) when
    is_integer(Uid), is_integer(Kind)
->
    case validate_v1_common(KindId, InfoSealed, KeyVersion, AttachHashes) of
        ok ->
            v1_tx(fun(Conn) ->
                ok = user_collect_repo:key_state_gate_tx(Conn, Uid, KeyVersion),
                ok = validate_attach_tx(Conn, Uid, AttachHashes),
                add_v1_step_tx(Conn, Uid, Kind, KindId, InfoSealed, KeyVersion, AttachHashes)
            end);
        {error, Code} ->
            {error, Code}
    end.

add_v1_step_tx(Conn, Uid, Kind, KindId, InfoSealed, KeyVersion, AttachHashes) ->
    case user_collect_repo:find_any_by_uid_kind_id_tx(Conn, Uid, KindId) of
        not_found ->
            ok = user_collect_repo:insert_sealed_tx(
                Conn, Uid, Kind, KindId, InfoSealed, KeyVersion, AttachHashes
            ),
            {ok, #{<<"generation">> => 1}};
        {ok, Row} ->
            case maps:get(<<"key_version">>, Row, null) of
                null ->
                    %% legacy 明文行不可由 v1 覆盖（升级路径 T2 属 M2）
                    throw({abort_tx, ?ERR_COLLECT_LEGACY_CONFLICT});
                _SealedKv ->
                    add_v1_sealed_existing_tx(
                        Conn, Row, Uid, KindId, InfoSealed, KeyVersion, AttachHashes
                    )
            end
    end.

add_v1_sealed_existing_tx(Conn, Row, Uid, KindId, InfoSealed, KeyVersion, AttachHashes) ->
    case maps:get(<<"status">>, Row, 1) of
        0 ->
            %% 墓碑复活：复用原行 CAS（generation=墓碑G+1），禁止 INSERT
            {ok, Gen} = v1_row_generation(Row),
            case
                user_collect_repo:revive_sealed_tx(
                    Conn, Uid, KindId, Gen, InfoSealed, KeyVersion, AttachHashes
                )
            of
                {ok, updated} ->
                    {ok, #{<<"generation">> => Gen + 1}};
                {ok, noop} ->
                    %% 并发复活双写：读行做幂等/冲突判定
                    v1_idempotent_or_conflict_tx(
                        Conn, Uid, KindId, Gen + 1, InfoSealed, KeyVersion, AttachHashes
                    )
            end;
        1 ->
            %% 已收藏：同完整持久化状态幂等成功，否则冲突（编辑走 update_v1）
            {ok, Gen} = v1_row_generation(Row),
            v1_idempotent_or_conflict_tx(
                Conn, Uid, KindId, Gen, InfoSealed, KeyVersion, AttachHashes
            )
    end.

%% @doc v1 编辑（CAS）：expected_generation 匹配方可更新，0 行时读行判定
%% 幂等（同代同密文同版本同引用）或 409 冲突。
-spec update_v1(integer(), binary(), integer(), integer(), binary(), integer(), [binary()]) ->
    {ok, map()} | {error, integer()}.
update_v1(Uid, KindId, ExpectedGen, NewGen, InfoSealed, KeyVersion, AttachHashes) when
    NewGen =:= ExpectedGen + 1
->
    case validate_v1_common(KindId, InfoSealed, KeyVersion, AttachHashes) of
        ok ->
            v1_tx(fun(Conn) ->
                ok = user_collect_repo:key_state_gate_tx(Conn, Uid, KeyVersion),
                ok = validate_attach_tx(Conn, Uid, AttachHashes),
                case
                    user_collect_repo:cas_update_sealed_tx(
                        Conn,
                        Uid,
                        KindId,
                        ExpectedGen,
                        NewGen,
                        InfoSealed,
                        KeyVersion,
                        AttachHashes
                    )
                of
                    {ok, updated} ->
                        {ok, #{<<"generation">> => NewGen}};
                    {ok, noop} ->
                        v1_idempotent_or_conflict_tx(
                            Conn, Uid, KindId, NewGen, InfoSealed, KeyVersion, AttachHashes
                        )
                end
            end);
        {error, Code} ->
            {error, Code}
    end;
update_v1(_Uid, _KindId, _ExpectedGen, _NewGen, _InfoSealed, _KeyVersion, _AttachHashes) ->
    %% 修订号必须逐次 +1（§7.3）
    {error, ?ERR_COLLECT_BAD_REQUEST}.

%% @doc v1 remove = sealed 墓碑化（与 legacy remove 共用 remove_dispatch/2）。

%% 幂等判定：行完整持久化状态（generation/info/key_version/attach）与提交
%% 一致 → 幂等成功；否则 generation 冲突（§7.3，幂等重放零副作用由触发器
%% 差集为空保证）。
-spec v1_idempotent_or_conflict_tx(pid(), integer(), binary(), integer(), binary(), integer(), [
    binary()
]) ->
    {ok, map()} | no_return().
v1_idempotent_or_conflict_tx(Conn, Uid, KindId, Gen, InfoSealed, KeyVersion, AttachHashes) ->
    case user_collect_repo:find_any_by_uid_kind_id_tx(Conn, Uid, KindId) of
        {ok, Row} ->
            AttachJoined = elib_cnv:implode(<<",">>, AttachHashes),
            case
                maps:get(<<"generation">>, Row, -1) =:= Gen andalso
                    maps:get(<<"info">>, Row, <<>>) =:= InfoSealed andalso
                    maps:get(<<"key_version">>, Row, null) =:= KeyVersion andalso
                    maps:get(<<"attach_file_hash256">>, Row, <<>>) =:= AttachJoined
            of
                true ->
                    {ok, #{<<"generation">> => Gen, <<"idempotent">> => true}};
                false ->
                    throw({abort_tx, ?ERR_COLLECT_GENERATION_CONFLICT})
            end;
        not_found ->
            throw({abort_tx, ?ERR_COLLECT_GENERATION_CONFLICT})
    end.

v1_row_generation(Row) ->
    case maps:get(<<"generation">>, Row, null) of
        Gen when is_integer(Gen) ->
            {ok, Gen};
        _ ->
            throw({abort_tx, ?ERR_COLLECT_BAD_REQUEST})
    end.

%% v1 事务包装：throw({abort_tx, Code}) → {error, Code}
v1_tx(Fun) ->
    case elib_pg:with_tx(Fun) of
        {error, key_version_rejected} ->
            {error, ?ERR_COLLECT_KEY_VERSION_REJECTED};
        {error, Code} when is_integer(Code) ->
            {error, Code};
        Other ->
            Other
    end.

%% @doc v1 公共参数校验（fail-closed，任一非法整单拒绝）。
-spec validate_v1_common(binary(), binary(), integer(), [binary()]) -> ok | {error, integer()}.
validate_v1_common(KindId, InfoSealed, KeyVersion, AttachHashes) ->
    Checks = [
        {
            is_binary(KindId) andalso KindId =/= <<>> andalso
                byte_size(KindId) =< ?V1_KIND_ID_MAX,
            ?ERR_COLLECT_BAD_REQUEST
        },
        {
            is_binary(InfoSealed) andalso InfoSealed =/= <<>> andalso
                byte_size(InfoSealed) =< ?V1_INFO_SEALED_MAX,
            ?ERR_COLLECT_BAD_REQUEST
        },
        {
            is_integer(KeyVersion) andalso KeyVersion >= 1 andalso
                KeyVersion =< 1000000,
            ?ERR_COLLECT_BAD_REQUEST
        },
        {
            is_list(AttachHashes) andalso length(AttachHashes) =< ?V1_ATTACH_MAX andalso
                lists:all(
                    fun(H) ->
                        is_binary(H) andalso H =/= <<>> andalso byte_size(H) =< 64
                    end,
                    AttachHashes
                ),
            ?ERR_COLLECT_BAD_REQUEST
        }
    ],
    case [Code || {Pass, Code} <- Checks, not Pass] of
        [] -> ok;
        [Code | _] -> {error, Code}
    end.

%% @doc §7.2 引用校验：每个哈希须解析到现存 attachment 行且请求者对该行
%% 有权（复用 attach_logic:authorize 全 scope 分发：public/private=creator/
%% c2c=会话双成员/group/channel/moment/talk_record）。任一失败整单拒绝。
-spec validate_attach_tx(pid(), integer(), [binary()]) -> ok | no_return().
validate_attach_tx(_Conn, _Uid, []) ->
    ok;
validate_attach_tx(Conn, Uid, Hashes) ->
    %% 逐哈希单查（参数化；引用上限 10，主键级代价可接受）：任一哈希
    %% 不存在或请求者无权 → 整单拒绝（fail-closed）。
    Sql =
        <<"SELECT file_hash256, scope, scope_ref, creator_user_id, path, status ",
            "FROM attachment WHERE file_hash256 = $1">>,
    lists:foreach(
        fun(H) ->
            case elib_pg:query(Conn, Sql, [H]) of
                {ok, []} ->
                    throw({abort_tx, ?ERR_COLLECT_ATTACH_INVALID});
                {ok, Rows} ->
                    Authorized =
                        lists:any(
                            fun(R) -> attach_logic:authorize(Uid, R) end,
                            Rows
                        ),
                    Authorized orelse
                        throw({abort_tx, ?ERR_COLLECT_ATTACH_INVALID});
                {error, _Reason} ->
                    throw({abort_tx, ?ERR_COLLECT_ATTACH_INVALID})
            end
        end,
        lists:usort(Hashes)
    ).

%% @doc sealed 行存在性（任意 status）——handler 守卫复用。
-spec sealed_exists(integer(), binary()) -> boolean().
sealed_exists(Uid, KindId) ->
    user_collect_ds:sealed_exists(Uid, KindId).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 确保参数为 map 类型（非 map 一律降级为空 map，避免下游 maps:get/find 抛 badmap）
%% @private
-spec ensure_map(any()) -> map().
ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(_) ->
    #{}.

%% @doc 获取附件信息
%% @private
-spec get_info(non_neg_integer(), binary(), binary(), map()) -> {map(), binary()} | {map(), ok}.
get_info(0, MimeType, Key, Info) ->
    PayloadIn = maps:get(<<"payload">>, Info, #{}),
    %% payload 可能是已解析 map，也可能是 JSON binary；JSON 还可能解出 list/scalar/null，
    %% 统一经 ensure_map 收口为 map，消除 jsone:decode 返回非 map 时 maps:find/maps:get 的 badmap 崩溃
    Payload =
        case PayloadIn of
            PBin when is_binary(PBin) -> ensure_map(jsone:decode(PBin, [{object_format, map}]));
            PM when is_map(PM) -> PM;
            _ -> #{}
        end,
    %% Key 缺失时降级为空附件（收藏本体仍保存），不能 badmatch 崩 500
    Uri =
        case maps:find(Key, Payload) of
            {ok, U} ->
                U;
            error ->
                ?WARN_LOG("collect payload missing key ~p, degrade to empty attach", [Key]),
                <<>>
        end,
    %% 双读兼容：新消息 metadata 用 file_hash256，旧消息用 md5
    Md5 = maps:get(<<"file_hash256">>, Payload, maps:get(<<"md5">>, Payload, <<>>)),
    Size = maps:get(<<"size">>, Payload, 0),

    UriList =
        case Uri of
            B when is_binary(B) -> binary_to_list(B);
            L when is_list(L) -> L;
            _ -> ""
        end,
    {UrlMap, _UpData} = elib_uri:get_params(UriList),
    Path = maps:get(path, UrlMap),

    % Uri = proplists:get_value(<<"uri">>, Info),
    Attach = #{
        <<"file_hash256">> => Md5,
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

%% @doc kind=2 多图（imageMulti）：payload.images 含有效哈希项时每张图产出
%% 一个 Attach——附件哈希经 attachment_hashes/1 逗号拼接落
%% user_collect.attach_file_hash256（列语义即逗号列表，迁移 26），逐行经
%% attachment_ds:save 递增 referer_time；引用守卫按 ANY(string_to_array)
%% 匹配（迁移 00000226 对齐）。空哈希项跳过（不产生 path='' 垃圾行）；
%% images 缺失/为空/全无效或已收藏（Count≠0）返回 single 走既有单图路径，
%% 行为逐字节不变。
%% @private
-spec collect_image_attaches(non_neg_integer(), map()) -> {multi, [map()]} | single.
collect_image_attaches(0, Info) ->
    Payload = collect_payload(Info),
    case maps:get(<<"images">>, Payload, undefined) of
        Images when is_list(Images), Images =/= [] ->
            Attaches = [
                image_attach(Item)
             || Item <- Images,
                is_map(Item),
                image_hash_valid(Item)
            ],
            case Attaches of
                [] -> single;
                _ -> {multi, Attaches}
            end;
        _ ->
            single
    end;
collect_image_attaches(_Count, _Info) ->
    % 已经收藏：add_kind 对 Count≠0 为 no-op，无需重新提取
    single.

%% @doc payload 收口（与 get_info/4 同一口径）：已解析 map 直接用，
%% JSON binary 解码，其余降级空 map。
%% @private
-spec collect_payload(map()) -> map().
collect_payload(Info) ->
    case maps:get(<<"payload">>, Info, #{}) of
        PBin when is_binary(PBin) ->
            ensure_map(jsone:decode(PBin, [{object_format, map}]));
        PM when is_map(PM) ->
            PM;
        _ ->
            #{}
    end.

%% @private
image_hash_valid(Item) ->
    case maps:get(<<"file_hash256">>, Item, maps:get(<<"md5">>, Item, <<>>)) of
        Hash when is_binary(Hash), Hash =/= <<>> -> true;
        _ -> false
    end.

%% @doc 单张图片元素 → Attach（形态与 get_info/4 单图产物一致；
%% 双读兼容：优先 file_hash256，回退 md5）。
%% @private
-spec image_attach(map()) -> map().
image_attach(Item) ->
    Uri = maps:get(<<"uri">>, Item, <<>>),
    UriList =
        case Uri of
            B when is_binary(B) -> binary_to_list(B);
            L when is_list(L) -> L;
            _ -> ""
        end,
    {UrlMap, _UpData} = elib_uri:get_params(UriList),
    #{
        <<"file_hash256">> => maps:get(
            <<"file_hash256">>, Item, maps:get(<<"md5">>, Item, <<>>)
        ),
        <<"mime_type">> => <<"image/jpeg">>,
        <<"name">> => maps:get(<<"name">>, Item, <<>>),
        <<"path">> => maps:get(path, UrlMap),
        <<"url">> => Uri,
        <<"size">> => maps:get(<<"size">>, Item, 0)
    }.

-spec add_kind(integer(), integer(), binary(), binary(), binary(), binary(), binary(), list()) ->
    ok | {error, term()} | {rollback, term()}.
add_kind(0, Uid, KindBin, KindId, Info, Source, Remark, Attach) ->
    NowTs = elib_dt:now(),
    Kind = binary_to_integer(KindBin),
    elib_pg:with_tx(fun(Conn) ->
        attachment_ds:save(Conn, NowTs, Uid, Attach),
        Data = #{
            <<"id">> => elib_tsid:generate(user_collect),
            <<"user_id">> => Uid,
            <<"kind">> => Kind,
            <<"kind_id">> => KindId,
            <<"source">> => Source,
            <<"remark">> => Remark,
            <<"info">> => elib_hasher:encoded_val(Info),
            <<"attach_file_hash256">> => attachment_hashes(Attach),
            <<"created_at">> => NowTs,
            <<"updated_at">> => NowTs,
            <<"status">> => 1
        },

        insert_collect_tx(Conn, Data)
    end);
add_kind(_Count, _Kind, _Uid, _KindId, _Info, _Source, _Remark, _) ->
    % 已收藏
    ok.

attachment_hashes(Attach) ->
    Hashes = [
        maps:get(<<"file_hash256">>, Item, maps:get(<<"md5">>, Item, <<>>))
     || Item <- Attach
    ],
    case Hashes of
        [] -> <<>>;
        _ -> elib_cnv:implode(<<",">>, Hashes)
    end.

insert_collect_tx(Conn, Data) ->
    {Sql, Params} = elib_pg_sql:insert(<<"public.user_collect">>, Data, <<>>),
    OnConflictUpdate = <<
        "ON CONFLICT (user_id, status, kind_id) DO UPDATE SET "
        "updated_at = EXCLUDED.updated_at, "
        "status = 1"
    >>,
    FullSql = [Sql, <<" ">>, OnConflictUpdate],
    case elib_pg:execute(Conn, FullSql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({collect_insert_failed, Reason})
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(TEST).

%% payload 缺目标键时降级为空附件（收藏本体仍保存），不 badmatch 崩 500
get_info_missing_key_degrades_test() ->
    {Attach, Info2} = get_info(0, <<"image/jpeg">>, <<"uri">>, #{<<"payload">> => #{}}),
    ?assertEqual(<<>>, maps:get(<<"url">>, Attach)),
    ?assert(is_binary(Info2)).

%% payload 为非 map（JSON 解出 list/scalar）时同样降级，不 badmap 崩溃
get_info_non_map_payload_degrades_test() ->
    {Attach, _} = get_info(0, <<"image/jpeg">>, <<"uri">>, #{<<"payload">> => [1, 2]}),
    ?assertEqual(<<>>, maps:get(<<"url">>, Attach)).

%% kind=2 多图：payload.images 每图一个 Attach，file_hash256 双读 md5，
%% 无哈希项（含纯 uri 项）跳过
collect_image_attaches_multi_test() ->
    Info = #{
        <<"payload">> => #{
            <<"images">> => [
                #{<<"uri">> => <<"u1/a.jpg">>, <<"file_hash256">> => <<"h1">>, <<"size">> => 100},
                #{<<"uri">> => <<"u1/b.jpg">>, <<"md5">> => <<"h2">>, <<"size">> => 200},
                #{<<"uri">> => <<"u1/c.jpg">>},
                <<"not-a-map">>
            ]
        }
    },
    {multi, Attaches} = collect_image_attaches(0, Info),
    ?assertEqual(2, length(Attaches)),
    ?assertEqual(<<"h1">>, maps:get(<<"file_hash256">>, lists:nth(1, Attaches))),
    ?assertEqual(<<"h2">>, maps:get(<<"file_hash256">>, lists:nth(2, Attaches))),
    ?assertEqual(<<"u1/b.jpg">>, maps:get(<<"url">>, lists:nth(2, Attaches))),
    ?assertEqual(<<"image/jpeg">>, maps:get(<<"mime_type">>, lists:nth(1, Attaches))).

%% images 全部无有效哈希 → single（回退单图路径，不产生垃圾行）
collect_image_attaches_all_invalid_test() ->
    ?assertEqual(
        single,
        collect_image_attaches(0, #{
            <<"payload">> => #{
                <<"images">> => [
                    #{<<"uri">> => <<"u1/a.jpg">>},
                    #{<<"file_hash256">> => <<>>}
                ]
            }
        })
    ).

%% 旧单图（无 images）/ images 空列表 / payload 非 map / 无 payload → single
collect_image_attaches_single_test() ->
    ?assertEqual(
        single,
        collect_image_attaches(0, #{<<"payload">> => #{<<"uri">> => <<"u1/a.jpg">>}})
    ),
    ?assertEqual(single, collect_image_attaches(0, #{<<"payload">> => #{<<"images">> => []}})),
    ?assertEqual(single, collect_image_attaches(0, #{<<"payload">> => [1, 2]})),
    ?assertEqual(single, collect_image_attaches(0, #{})).

%% payload 为 JSON binary 同样解出多图；已收藏（Count≠0）恒 single
collect_image_attaches_binary_and_recollect_test() ->
    PayloadBin = jsone:encode(#{
        <<"images">> => [
            #{<<"uri">> => <<"u1/a.jpg">>, <<"file_hash256">> => <<"h1">>}
        ]
    }),
    {multi, [Attach]} = collect_image_attaches(0, #{<<"payload">> => PayloadBin}),
    ?assertEqual(<<"h1">>, maps:get(<<"file_hash256">>, Attach)),
    ?assertEqual(single, collect_image_attaches(1, #{<<"payload">> => PayloadBin})).

-endif.
