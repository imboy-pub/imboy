-module(group_member_logic).
%%%
% group_member 业务逻辑模块
%%%
-export([join/4]).
-export([leave/4]).
-export([alias/4]).

-include_lib("imlib/include/log.hrl").


join(_, _, 0, _) ->
    {error, "群不存在，或者群ID有误。"};
join(_, _, Max, Count) when Max =< Count ->
    {error, "群成员已满。"};
join(Uid, Gid, _, _) ->
    Now = imboy_dt:utc(millisecond),
    imboy_db:with_transaction(fun(Conn) ->
        group_member_repo:add(Conn, #{
            group_id => Gid,
            user_id => Uid,
            role => 1, % 角色: 1 成员  2 嘉宾  3  管理员 4 群主
            is_join => 1,
            created_at => Now
        }),
        Data = #{
            member_count => {raw, <<"member_count+1">>},
            updated_at => imboy_dt:utc(millisecond)
        },
        imboy_db:update(Conn
            , group_repo:tablename()
            , <<"id = ", (ec_cnv:to_binary(Gid))/binary>>
            , Data
        ),
        group_member_ds:flush_cache(Gid),
        % TODO 通知群成员更新信息
        ok
    end),
    ok.

leave(_, _, GMSize, _) when GMSize == 0 ->
    ok;
leave(Uid, Gid, _, GM) ->
    Now = imboy_dt:utc(millisecond),
    Id = maps:get(<<"id">>, GM, 0),

    imboy_db:with_transaction(fun(Conn) ->
        Tb2 = group_member_repo:tablename(),
        Sql2 = <<"DELETE FROM ", Tb2/binary, " WHERE id= ", (ec_cnv:to_binary(Id))/binary>>,
        imboy_db:execute(Conn, Sql2, []),

        {ok, Body} = jsone_encode:encode(GM, [native_utf8]),
        group_log_repo:add(Conn, #{
            % 日志类型: 100 群转让 101 群解散  200 主动退出群   201 群解散退出群  202 被踢出群
            type => 200,
            option_uid => Uid,
            group_id => Gid,
            body => Body,
            created_at => Now
            }),
        Data = #{
            member_count => {raw, <<"member_count-1">>},
            updated_at => Now
        },
        imboy_db:update(Conn
            , group_repo:tablename()
            , <<"id = ", (ec_cnv:to_binary(Gid))/binary>>
            , Data
        ),
        group_member_ds:flush_cache(Gid),
        % TODO 通知群成员更新信息
        ok
    end),
    ok.

alias(Uid, Gid, Alias, Description) ->
    Now = imboy_dt:utc(millisecond),
    Data = #{
        alias => Alias,
        description => Description,
        updated_at => Now
    },
    imboy_db:update(
        group_member_repo:tablename()
        , <<"group_id = ", (ec_cnv:to_binary(Gid))/binary, " AND user_id = ", (ec_cnv:to_binary(Uid))/binary>>
        , Data
    ),
    % TODO 通知群成员更新信息
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================



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
