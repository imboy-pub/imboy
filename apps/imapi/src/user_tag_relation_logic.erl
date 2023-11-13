-module(user_tag_relation_logic).
%%%
% user_tag_relation 业务逻辑模块
% user_tag_relation business logic module
%%%

-export([add/4]).
-export([remove/4]).
-export([set/5]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").


%% ===================================================================
%% API
%% ===================================================================
-spec remove(integer(), binary(), integer(), binary()) -> ok.
remove(Uid, Scene, ObjectId, TagId) when is_integer(TagId) ->
    remove(Uid, Scene, ObjectId, integer_to_binary(TagId));
remove(Uid, Scene, ObjectId, TagId) ->
    Uid2 = integer_to_binary(Uid),
    % TagName = <<"aaa">>,
    TagName = imboy_db:pluck(<<"user_tag">>, <<"id = ", TagId/binary>>, <<"name">>, <<>>),
    imboy_db:with_transaction(fun(Conn) ->
                                     % 移除 public.user_tag_relation
                                     user_tag_relation_repo:remove_user_tag_relation(Conn,
                                                                                     Scene,
                                                                                     Uid2,
                                                                                     TagId,
                                                                                     ObjectId),
                                     user_tag_relation_repo:replace_object_tag(Conn,
                                                                               Scene,
                                                                               Uid2,
                                                                               ObjectId,
                                                                               TagName,
                                                                               <<>>),
                                     ok
                              end),
    % 清理缓存
    user_tag_relation_repo:flush_subtitle(TagId),
    ok.


%% 用户标签_联系人标签设置标签
-spec set(integer(), binary(), list(), integer(), binary()) -> ok.
set(Uid, Scene, ObjectIds, TagId, TagName) when is_integer(TagId) ->
    set(Uid, Scene, ObjectIds, integer_to_binary(TagId), TagName);
set(Uid, Scene, ObjectIds, TagId, TagName) ->
    NowTs = imboy_dt:millisecond(),
    Uid2 = integer_to_binary(Uid),
    CreatedAt = integer_to_binary(NowTs),

    Check = imboy_db:pluck(<<"user_tag">>,
                           <<"scene = ", Scene/binary, " AND creator_user_id = ", Uid2/binary, " AND name = '",
                             TagName/binary, "' AND id != ", TagId/binary>>,
                           <<"count(*)">>,
                           0),
    if
        Check > 0 ->
            <<TagName/binary, " 已存在"/utf8>>;
        true ->
            % [imboy_hashids:uid_encode(108), imboy_hashids:uid_encode(62902), imboy_hashids:uid_encode(62903)].
            ObjectIds2 = [integer_to_binary(imboy_hashids:uid_decode(I)) ||
                             I <- ObjectIds, imboy_hashids:uid_decode(I) > 0],
            Tb = imboy_db:public_tablename(<<"user_friend">>),
            OldObjectIds = imboy_db:list(<<"SELECT to_user_id::text FROM ", Tb/binary, " WHERE tag like '%",
                                           TagName/binary, ",%'">>),
            OldObjectIds2 = [Id || {Id} <- OldObjectIds],

            DelObjectId = OldObjectIds2 -- ObjectIds2,

            imboy_db:with_transaction(fun(Conn) ->
                                             %
                                             [user_tag_relation_repo:remove_user_tag_relation(Conn,
                                                                                              Scene,
                                                                                              Uid2,
                                                                                              TagId,
                                                                                              I) || I <- DelObjectId],
                                             % imboy_log:info(io_lib:format("user_tag_relation_repo:set/5 ObjectIds2:~p, RefCount, ~p;~n", [ObjectIds2, [Conn , TagId,TagName, RefCount, Uid2, CreatedAt]])),
                                             % 保存 public.user_tag
                                             user_tag_relation_repo:update_tag(Conn, TagId, TagName, Uid2, CreatedAt),

                                             % 插入 public.user_tag_relation
                                             [user_tag_relation_repo:save_user_tag_relation(Conn,
                                                                                            Scene,
                                                                                            Uid2,
                                                                                            TagId,
                                                                                            I,
                                                                                            CreatedAt) ||
                                                 I <- ObjectIds2, I > 0],

                                             [user_tag_logic:change_scene_tag(Conn,
                                                                              Scene,
                                                                              Uid2,
                                                                              I,
                                                                              [{TagId, TagName}]) || I <- ObjectIds2],
                                             % Conn, Scene, Uid2, ObjectId, FromName, ToName
                                             %
                                             [user_tag_relation_repo:replace_object_tag(Conn,
                                                                                        Scene,
                                                                                        Uid2,
                                                                                        I,
                                                                                        TagName,
                                                                                        <<>>) || I <- DelObjectId],
                                             ok
                                      end),
            % 清理缓存
            user_tag_relation_repo:flush_subtitle(TagId),
            ok
    end.


%%% 添加标签
-spec add(integer(), binary(), binary(), list()) -> ok.
add(Uid, Scene, <<>>, [Tag]) ->
    imboy_log:info(io_lib:format("user_tag_relation_logic:add/3 uid ~p scene ~p, tag: ~p; ~n", [Uid, Scene, Tag])),
    Count = imboy_db:pluck(<<"user_tag">>, <<"scene = ", Scene/binary, " AND name = ", Tag/binary>>, <<"id">>, 0),
    case Count of
        0 ->
            Column = <<"(creator_user_id,scene,name,referer_time,created_at)">>,
            Value = [Uid, Scene, <<"'", Tag/binary, "'">>, 0, imboy_dt:millisecond()],
            imboy_db:insert_into(<<"user_tag">>, Column, Value),
            ok;
        _ ->
            <<"标签名已存在"/utf8>>
    end;

add(Uid, <<"1">>, ObjectId, Tag) ->
    do_add(<<"1">>, Uid, ObjectId, Tag),
    ok;
add(Uid, <<"2">>, ObjectId, Tag) when is_integer(ObjectId) ->
    do_add(<<"2">>, Uid, ObjectId, Tag),
    ok;
add(Uid, <<"2">>, ObjectId, Tag) ->
    do_add(<<"2">>, Uid, imboy_hashids:uid_decode(ObjectId), Tag),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

do_add(Scene, Uid, ObjectId, Tag) when is_integer(ObjectId) ->
    do_add(Scene, Uid, integer_to_binary(ObjectId), Tag);

% Tag = [] 移除特定对象的标签
do_add(Scene, Uid, ObjectId, []) ->
    Uid2 = integer_to_binary(Uid),
    imboy_db:with_transaction(fun(Conn) ->
                                     {Table, Where} =
                                         case Scene of
                                             <<"1">> ->
                                                 {imboy_db:public_tablename(<<"user_collect">>),
                                                  <<"user_id = ", Uid2/binary, " AND kind_id = '", ObjectId/binary,
                                                    "'">>};
                                             <<"2">> ->
                                                 {imboy_db:public_tablename(<<"user_friend">>),
                                                  <<"from_user_id = ", Uid2/binary, " AND to_user_id = ",
                                                    ObjectId/binary>>}
                                         end,
                                     Sql = <<"UPDATE ", Table/binary, " SET tag = '' WHERE ", Where/binary>>,
                                     % imboy_log:info(io_lib:format("user_tag_relation_logic:do_add/4 sql ~p; ~n", [Sql])),
                                     epgsql:equery(Conn, Sql),

                                     % 删除 public.user_tag_relation
                                     delete_object_tag(Conn, Scene, Uid2, ObjectId),
                                     ok
                              end),
    ok;
%
do_add(Scene, Uid, ObjectId, Tag) ->
    % check public.user_tag
    % {ok,[<<"id">>,<<"name">>],[{1,<<"a">>},{4,<<"b">>}]}
    % {ok, _, Tag2} = user_tag_relation_repo:select_tag(
    %     <<"scene = ", Scene/binary, " AND name = any(string_to_array($1, ','))">>
    %     , [imboy_func:implode(",",Tag)]
    %     , <<"id, name">>
    % ),
    % TagIdNewLi = [Id || {Id, _} <- Tag2],
    % imboy_log:info(io_lib:format("user_tag_relation_logic:add/4 TagIdNewLi:~p;~n", [TagIdNewLi])),
    NowTs = imboy_dt:millisecond(),
    Uid2 = integer_to_binary(Uid),
    CreatedAt = integer_to_binary(NowTs),
    imboy_db:with_transaction(fun(Conn) ->
                                     % 删除 public.user_tag_relation
                                     delete_object_tag(Conn, Scene, Uid2, ObjectId),

                                     % 插入 public.user_tag
                                     TagIdNewLi = [user_tag_relation_repo:save_tag(Conn,
                                                                                   Uid2,
                                                                                   Scene,
                                                                                   CreatedAt,
                                                                                   Name) || Name <- Tag],

                                     % imboy_log:info(io_lib:format("user_tag_relation_logic:add/4 TagIdNewLi:~p;~n", [TagIdNewLi])),

                                     % 插入 public.user_tag_relation
                                     [user_tag_relation_repo:save_user_tag_relation(Conn,
                                                                                    Scene,
                                                                                    Uid2,
                                                                                    integer_to_binary(TagId),
                                                                                    ObjectId,
                                                                                    CreatedAt) ||
                                         {TagId, _Name} <- TagIdNewLi, TagId > 0],
                                     % change_scene_tag(Conn, Scene, Uid2, ObjectId, Tag),
                                     [user_tag_logic:change_scene_tag(Conn,
                                                                      Scene,
                                                                      Uid2,
                                                                      ObjectId,
                                                                      [{integer_to_binary(TagId), N}]) ||
                                         {TagId, N} <- TagIdNewLi, TagId > 0],

                                     % 清理缓存
                                     [user_tag_relation_repo:flush_subtitle(TagId) || {TagId, _} <- TagIdNewLi],

                                     ok
                              end),
    ok.


% 删除 public.user_tag_relation
delete_object_tag(Conn, Scene, Uid, ObjectId) ->
    DelTb = user_tag_relation_repo:tablename(),
    DelWhere = <<"scene = ", Scene/binary, " AND user_id = ", Uid/binary, " AND object_id = '", ObjectId/binary, "'">>,

    DelItems = imboy_db:list(Conn, <<"SELECT tag_id FROM ", DelTb/binary, " WHERE ", DelWhere/binary>>),

    % imboy_log:info(io_lib:format("user_tag_relation_logic:delete_object_tag/4 DelItems ~p; ~n", [DelItems])),

    DelSql = <<"DELETE FROM ", DelTb/binary, " WHERE ", DelWhere/binary>>,
    % imboy_log:info(io_lib:format("user_tag_relation_logic:delete_object_tag/4 DelSql ~p; ~n", [DelSql])),
    % Res = epgsql:equery(Conn, DelSql, []),

    {ok, Stmt} = epgsql:parse(Conn, DelSql),
    epgsql:execute_batch(Conn, [{Stmt, []}]),
    % Res = epgsql:execute_batch(Conn, [{Stmt, []}]),
    % imboy_log:error(io_lib:format("user_tag_relation_repo:delete_object_tag/4 Res:~p ~n", [Res])),

    % 清理缓存
    [user_tag_relation_repo:flush_subtitle(TagId) || {TagId} <- DelItems],
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
