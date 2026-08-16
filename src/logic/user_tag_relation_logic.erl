-module(user_tag_relation_logic).
%%%
% user_tag_relation 业务逻辑模块
% user_tag_relation business logic module
%%%

%% @doc 添加用户标签关系
%% 为对象添加标签，支持收藏和好友场景
%% @param Uid 用户ID
%% @param Scene 场景（1-收藏，2-好友）
%% @param ObjectId 对象ID
%% @param Tag 标签列表或单个标签
%% @returns ok | binary()
-export([add/4]).
-export([list/2]).

%% @doc 移除用户标签关系
%% 从对象上移除指定标签
%% @param Uid 用户ID
%% @param Scene 场景
%% @param ObjectId 对象ID
%% @param TagId 标签ID
%% @returns ok
-export([remove/4]).

%% @doc 设置用户标签关系
%% 批量设置对象的标签，支持新增和删除
%% @param Uid 用户ID
%% @param Scene 场景
%% @param ObjectIds 对象ID列表
%% @param TagId 标签ID
%% @param TagName 标签名称
%% @returns ok | binary()
-export([set/5]).

%% @doc 删除对象标签
%% @private
-export([delete_object_tag/4]).

%% @doc 检查两个用户是否为好友关系（friend scene 标签操作前置校验）
-export([check_friend/2]).

%% @doc 按标签分页查询好友列表
-export([friend_page_by_tag/5]).

%% @doc 按标签分页查询收藏列表（含标签名查找）
-export([collect_page/5]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================
-spec remove(integer(), binary() | integer(), integer(), integer()) -> ok.
remove(Uid, Scene, ObjectId, TagId) when is_binary(TagId) ->
    remove(Uid, Scene, ObjectId, binary_to_integer(TagId));
remove(Uid, Scene, ObjectId, TagId) ->
    % Scene 转 integer：epgsql parse 本地推断参数类型，binary 场景值编码
    % int4 会 integer_overflow 崩连接（生产 500 实证），与 set/5 全 integer 风格统一
    Scene2 = ec_cnv:to_integer(Scene),
    % 使用 elib_pg:one 替代 pluck
    Tb = elib_pg_sql:public_tablename(<<"user_tag">>),
    Sql = <<"SELECT name FROM ", Tb/binary, " WHERE id = $1">>,
    TagName =
        case elib_pg:one(Sql, [TagId]) of
            {ok, #{<<"name">> := Name}} -> Name;
            _ -> <<>>
        end,
    _ = elib_pg:with_tx(fun(Conn) ->
        % 移除 public.user_tag_relation
        user_tag_relation_ds:remove_user_tag_relation(
            Conn,
            Scene2,
            Uid,
            TagId,
            ObjectId
        ),
        user_tag_relation_ds:replace_object_tag(
            Conn,
            Scene2,
            Uid,
            ObjectId,
            binary_to_list(TagName),
            []
        ),
        ok
    end),
    % 清理缓存
    user_tag_relation_ds:flush_subtitle(TagId),
    ok.

%% 用户标签_联系人标签设置标签
-spec set(integer(), integer(), list(), integer(), binary()) -> ok | binary().
set(Uid, Scene, ObjectIds, TagId, TagName) when is_binary(TagId) ->
    set(Uid, Scene, ObjectIds, binary_to_integer(TagId), TagName);
set(Uid, Scene, ObjectIds, TagId, TagName) ->
    NowTs = elib_dt:now(),
    CreatedAt = NowTs,

    % 使用安全的参数化查询，避免SQL注入
    Tb2 = elib_pg_sql:public_tablename(<<"user_tag">>),
    CheckSql =
        <<"SELECT count(*) FROM ", Tb2/binary,
            " WHERE scene = $1 AND creator_user_id = $2 AND name = $3 AND id != $4">>,
    Check =
        case elib_pg:query(CheckSql, [Scene, Uid, TagName, TagId]) of
            {ok, [#{<<"count">> := Count}]} -> Count;
            _ -> 0
        end,
    if
        Check > 0 ->
            <<TagName/binary, " 已存在"/utf8>>;
        true ->
            ObjectIds2 = [
                integer_to_binary(ec_cnv:to_integer(I))
             || I <- ObjectIds, ec_cnv:to_integer(I) > 0
            ],
            Tb = elib_pg_sql:public_tablename(<<"user_friend">>),
            % 使用安全的参数化查询，避免SQL注入
            %% IDOR 防御：必须按 from_user_id = Uid 限定，否则会跨用户读取
            %% 其他用户 user_friend.tag 命中同一标签名的好友关系，混入本次
            %% 新旧对象差异计算（DelObjectId），导致多用户使用同名标签时
            %% 增删逻辑计算错乱。
            OldSql =
                <<"SELECT to_user_id::text FROM ", Tb/binary,
                    " WHERE from_user_id = $1 AND tag LIKE $2">>,
            {ok, OldObjectIds} = elib_pg:query(OldSql, [Uid, <<TagName/binary, ",%">>]),
            OldObjectIds2 = [maps:get(<<"to_user_id">>, Row) || Row <- OldObjectIds],

            DelObjectId = OldObjectIds2 -- ObjectIds2,

            _ = elib_pg:with_tx(fun(Conn) ->
                %
                [
                    user_tag_relation_ds:remove_user_tag_relation(
                        Conn,
                        Scene,
                        Uid,
                        TagId,
                        I
                    )
                 || I <- DelObjectId
                ],
                % elib_log:info(io_lib:format("user_tag_relation_ds:set/5 ObjectIds2:~p, RefCount, ~p;~n", [ObjectIds2, [Conn , TagId,TagName, RefCount, Uid, CreatedAt]])),
                % 保存 public.user_tag
                _ = user_tag_relation_ds:update_tag(Conn, TagId, TagName, Uid, CreatedAt),

                % 插入 public.user_tag_relation
                [
                    user_tag_relation_ds:save_user_tag_relation(
                        Conn,
                        Scene,
                        Uid,
                        TagId,
                        I,
                        CreatedAt
                    )
                 || I <- ObjectIds2, I > 0
                ],

                [
                    user_tag_ds:change_scene_tag(
                        Conn,
                        Scene,
                        Uid,
                        I,
                        [{TagId, TagName}]
                    )
                 || I <- ObjectIds2
                ],
                % Conn, Scene, Uid, ObjectId, FromName, ToName
                %
                [
                    user_tag_relation_ds:replace_object_tag(
                        Conn,
                        Scene,
                        Uid,
                        I,
                        binary_to_list(TagName),
                        []
                    )
                 || I <- DelObjectId
                ],
                ok
            end),
            % 清理缓存
            user_tag_relation_ds:flush_subtitle(TagId),
            ok
    end.

%%% 添加标签
-spec add(integer(), integer(), binary() | integer(), list()) -> ok | binary().
add(Uid, Scene, <<>>, [Tag]) ->
    % 使用安全的参数化查询，避免SQL注入
    Tb = elib_pg_sql:public_tablename(<<"user_tag">>),
    Count =
        case
            elib_pg:one(<<"SELECT id FROM ", Tb/binary, " WHERE scene = $1 AND name = $2">>, [
                Scene, Tag
            ])
        of
            {ok, #{<<"id">> := _}} ->
                1;
            _ ->
                0
        end,
    case Count of
        0 ->
            _ = elib_pg:insert(Tb, #{
                creator_user_id => Uid,
                scene => Scene,
                name => Tag,
                referer_time => 0,
                created_at => elib_dt:now()
            }),
            ok;
        _ ->
            <<"标签名已存在."/utf8>>
    end;
add(Uid, 1, ObjectId, Tag) ->
    do_add(1, Uid, ObjectId, Tag),
    ok;
add(Uid, 2, ObjectId, Tag) when is_integer(ObjectId) ->
    do_add(2, Uid, ObjectId, normalize_friend_tags(Uid, Tag)),
    ok;
add(Uid, 2, ObjectId, Tag) ->
    do_add(2, Uid, ec_cnv:to_integer(ObjectId), normalize_friend_tags(Uid, Tag)),
    ok.

%% @doc 兼容旧入口：列出好友对象的标签关系
-spec list(integer(), integer() | binary()) -> {ok, list(map())} | {error, term()}.
list(Uid, ObjectId) when is_binary(ObjectId) ->
    list(Uid, ec_cnv:to_integer(ObjectId));
list(Uid, ObjectId) ->
    Where = <<"scene = $1 AND user_id = $2 AND object_id = $3">>,
    Column = <<"id, tag_id, object_id, created_at">>,
    user_tag_relation_ds:select_user_tag_relation(
        Where,
        [2, Uid, integer_to_binary(ObjectId)],
        Column
    ).

%% @doc 检查两个用户是否为好友关系
%% 用于 friend scene 下标签添加的前置验证
-spec check_friend(integer(), integer()) -> boolean().
check_friend(FromUid, ToUid) ->
    friend_ds:is_friend(FromUid, ToUid).

%% @doc 按标签分页查询好友列表
%% 直接返回分页 Payload map
-spec friend_page_by_tag(integer(), integer(), integer(), integer(), binary()) -> map().
friend_page_by_tag(Uid, Page, Size, TagId, Kwd) ->
    friend_ds:page_by_tag(Uid, Page, Size, TagId, Kwd).

%% @doc 按标签分页查询收藏列表
%% 先根据 TagId 和 Uid 查出 collect scene 下的标签名，再按标签名 LIKE 分页查询收藏
-spec collect_page(integer(), integer(), integer(), integer(), binary()) -> map().
collect_page(Uid, Page, Size, TagId, Kwd) ->
    TagName = user_tag_ds:find_name_by_id(TagId, Uid, 1),
    case TagName of
        <<>> ->
            #{total => 0, page => Page, size => Size, list => []};
        _ ->
            collect_page_by_tagname(Uid, Page, Size, TagName, Kwd)
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

-spec collect_page_by_tagname(integer(), integer(), integer(), binary(), binary()) -> map().
collect_page_by_tagname(Uid, Page, Size, TagName, Kwd) ->
    TagPattern = <<"%", TagName/binary, ",%">>,
    BaseWhere = #{
        user_id => Uid,
        status => 1,
        tag => {op, <<"LIKE">>, TagPattern}
    },
    WhereMap =
        case byte_size(Kwd) > 0 of
            true ->
                Like = <<"%", Kwd/binary, "%">>,
                BaseWhere#{
                    <<"__or">> => [
                        #{source => {op, <<"LIKE">>, Like}},
                        #{remark => {op, <<"LIKE">>, Like}},
                        #{info => {op, <<"LIKE">>, Like}}
                    ]
                };
            false ->
                BaseWhere
        end,
    % A-05：info 取原始列，解密在应用层做（历史实现把主密钥内联进 SQL 列表达式）
    Column = <<"kind, kind_id, source, created_at, updated_at, tag, info">>,
    case user_collect_ds:page(Column, WhereMap, <<"id desc">>, Page, Size) of
        {ok, Payload} ->
            List = elib_hasher:decode_list_field(maps:get(list, Payload, []), <<"info">>),
            Payload#{list => elib_response:json_decode_list_field(List, <<"info">>)};
        {error, _Reason} ->
            #{total => 0, page => Page, size => Size, list => []}
    end.

-spec normalize_friend_tags(integer(), list()) -> list().
normalize_friend_tags(Uid, TagList) when is_list(TagList) ->
    [normalize_friend_tag(Uid, Tag) || Tag <- TagList];
normalize_friend_tags(_Uid, TagList) ->
    TagList.

-spec normalize_friend_tag(integer(), term()) -> term().
normalize_friend_tag(Uid, Tag) when is_binary(Tag) ->
    case maybe_tag_id(Tag) of
        {ok, TagId} ->
            case user_tag_ds:find_name_by_id(TagId, Uid) of
                <<>> -> Tag;
                Name -> Name
            end;
        error ->
            Tag
    end;
normalize_friend_tag(_Uid, Tag) ->
    Tag.

-spec maybe_tag_id(binary()) -> {ok, integer()} | error.
maybe_tag_id(Tag) ->
    try
        {ok, binary_to_integer(Tag)}
    catch
        error:badarg ->
            error
    end.

-spec do_add(integer(), integer(), any(), any()) -> ok.
do_add(Scene, Uid, ObjectId, Tag) when is_integer(ObjectId) ->
    do_add(Scene, Uid, integer_to_binary(ObjectId), Tag);
do_add(Scene, Uid, ObjectId, []) ->
    _ = elib_pg:with_tx(fun(Conn) ->
        {Table, WhereSql, WhereParams} =
            case Scene of
                1 ->
                    {
                        elib_pg_sql:public_tablename(<<"user_collect">>),
                        <<"user_id = $1 AND kind_id = $2">>,
                        [Uid, ObjectId]
                    };
                2 ->
                    {
                        elib_pg_sql:public_tablename(<<"user_friend">>),
                        <<"from_user_id = $1 AND to_user_id = $2">>,
                        [Uid, ObjectId]
                    }
            end,
        Sql = <<"UPDATE ", Table/binary, " SET tag = '' WHERE ", WhereSql/binary>>,
        % elib_log:info(io_lib:format("user_tag_relation_logic:do_add/4 sql ~p; ~n", [Sql])),
        {ok, _} = elib_pg:execute(Sql, WhereParams),
        % 删除 public.user_tag_relation
        delete_object_tag(Conn, Scene, Uid, ObjectId),
        ok
    end),
    ok;
do_add(Scene, Uid, ObjectId, Tag) ->
    % check public.user_tag
    % {ok,[<<"id">>,<<"name">>],[{1,<<"a">>},{4,<<"b">>}]}
    % {ok, _, Tag2} = user_tag_relation_ds:select_tag(
    %     <<"scene = ", Scene/binary, " AND name = any(string_to_array($1, ','))">>
    %     , [elib_cnv:implode(",",Tag)]
    %     , <<"id, name">>
    % ),
    % TagIdNewLi = [Id || {Id, _} <- Tag2],
    % elib_log:info(io_lib:format("user_tag_relation_logic:add/4 TagIdNewLi:~p;~n", [TagIdNewLi])),
    NowTs = elib_dt:now(),
    CreatedAt = NowTs,
    _ = elib_pg:with_tx(fun(Conn) ->
        % 删除 public.user_tag_relation
        delete_object_tag(Conn, Scene, Uid, ObjectId),

        % 插入 public.user_tag
        TagIdNewLi = [
            user_tag_relation_ds:save_tag(
                Conn,
                Uid,
                Scene,
                CreatedAt,
                Name
            )
         || Name <- Tag
        ],

        % elib_log:info(io_lib:format("user_tag_relation_logic:add/4 TagIdNewLi:~p;~n", [TagIdNewLi])),

        % 插入 public.user_tag_relation
        [
            user_tag_relation_ds:save_user_tag_relation(
                Conn,
                Scene,
                Uid,
                TagId,
                ObjectId,
                CreatedAt
            )
         || {TagId, _Name} <- TagIdNewLi
        ],
        % change_scene_tag(Conn, Scene, Uid, ObjectId, Tag),
        [
            user_tag_ds:change_scene_tag(
                Conn,
                Scene,
                Uid,
                ObjectId,
                [{TagId, N}]
            )
         || {TagId, N} <- TagIdNewLi
        ],

        % 清理缓存
        [user_tag_relation_ds:flush_subtitle(TagId) || {TagId, _} <- TagIdNewLi],

        ok
    end),
    ok.

% 删除 public.user_tag_relation
-spec delete_object_tag(any(), integer(), integer(), binary()) -> ok.
delete_object_tag(Conn, Scene, Uid, ObjectId) ->
    DelTb = user_tag_relation_ds:tablename(),
    DelWhereSql = <<"scene = $1 AND user_id = $2 AND object_id = $3">>,
    DelWhereParams = [Scene, Uid, ObjectId],

    SelectSql = <<"SELECT tag_id FROM ", DelTb/binary, " WHERE ", DelWhereSql/binary>>,
    {ok, DelItems} = elib_pg:query(Conn, SelectSql, DelWhereParams),

    % elib_log:info(io_lib:format("user_tag_relation_logic:delete_object_tag/4 DelItems ~p; ~n", [DelItems])),

    DelSql = <<"DELETE FROM ", DelTb/binary, " WHERE ", DelWhereSql/binary>>,
    % elib_log:info(io_lib:format("user_tag_relation_logic:delete_object_tag/4 DelSql ~p; ~n", [DelSql])),
    {ok, _} = elib_pg:execute(Conn, DelSql, DelWhereParams),
    % elib_log:error(io_lib:format("user_tag_relation_ds:delete_object_tag/4 Res:~p ~n", [Res])),

    % 清理缓存
    [user_tag_relation_ds:flush_subtitle(maps:get(<<"tag_id">>, Row)) || Row <- DelItems],
    ok.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
