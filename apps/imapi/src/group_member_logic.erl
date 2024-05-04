-module(group_member_logic).
%%%
% group_member 业务逻辑模块
%%%
-export([join/4, join/5]).
-export([leave/3]).
-export([alias/4]).
-export([list_member/1, list_member/2]).


-include_lib("imlib/include/log.hrl").

% group_member_logic:list_member(40).
-spec list_member(integer()) -> list().
list_member(Gid) when is_integer(Gid) ->
    TbA = group_member_repo:tablename(),
    TbB = user_repo:tablename(),
    Sql = <<"select u.nickname,u.account,u.avatar,u.sign, gm.* from ", TbA/binary, " gm left join ", TbB/binary, " u on u.id = gm.user_id WHERE gm.group_id = ", (ec_cnv:to_binary(Gid))/binary>>,
    imboy_db:query(Sql).

-spec list_member(integer(), list()) -> list().
list_member(Gid, MemberUids) when is_list(MemberUids) ->
    TbA = group_member_repo:tablename(),
    TbB = user_repo:tablename(),
    Bin = imboy_cnv:list_to_binary_string(MemberUids),
    Sql = <<"select u.nickname,u.account,u.avatar,u.sign, gm.* from ", TbA/binary, " gm left join ", TbB/binary, " u on u.id = gm.user_id WHERE gm.group_id = ", (ec_cnv:to_binary(Gid))/binary, " and gm.user_id in(", Bin/binary,");">>,
    ?LOG([Sql]),
    imboy_db:query(Sql).

join(_,_, _, 0, _) ->
    {error, "群不存在，或者群ID有误。"};
join(_,_, _, Max, Count) when Max =< Count ->
    {error, "群成员已满。"};
join(JoinMode,Uid, Gid, _, _) ->
    imboy_db:with_transaction(fun(Conn) ->
        join(Conn, JoinMode, Uid, Gid)
    end),
    ok.

join(Conn, JoinMode, Uid, Gid) ->
    Now = imboy_dt:utc(millisecond),
    ToUidLi = group_ds:member_uids(Gid),
    % ?LOG(ToUidLi),
    group_member_repo:add(Conn, #{
        group_id => Gid,
        user_id => Uid,
        role => 1, % 角色: 1 成员  2 嘉宾  3  管理员 4 群主
        is_join => 1,
        join_mode => JoinMode,
        created_at => Now
    }),
    Data = #{
        member_count => {raw, <<"member_count+1">>},
        user_id_sum => {raw, <<"user_id_sum+", (ec_cnv:to_binary(Uid))/binary>>},
        updated_at => imboy_dt:utc(millisecond)
    },
    imboy_db:update(Conn
        , group_repo:tablename()
        , <<"id = ", (ec_cnv:to_binary(Gid))/binary>>
        , Data
    ),
    group_ds:join(Uid, Gid),
    Sum = imboy_db:pluck(group_repo:tablename(),
        <<"id = ",  (ec_cnv:to_binary(Gid))/binary>>,
        <<"user_id_sum">>,
        0
    ),
    User = user_repo:find_by_id(Uid, <<"account,avatar,nickname">>),
    Payload = #{
        <<"gid">> => imboy_hashids:encode(Gid),
        <<"user_id_sum">> => Sum,
        <<"nickname">> => maps:get(<<"nickname">>, User),
        <<"avatar">> => maps:get(<<"avatar">>, User),
        <<"account">> => maps:get(<<"account">>, User),
        <<"msg_type">> => <<"group_member_join">>
    },
    msg_s2c_ds:send(Uid, Payload, ToUidLi, nosave),
    ok.

leave(Uid, Gid, CurrentUid) ->
    GM = group_member_repo:find(Gid, Uid, <<"*">>),
    GMSize = maps:size(GM),
    leave(Uid, Gid, GMSize, GM, CurrentUid).

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
    ToUidLi = group_ds:member_uids(Gid),
    msg_s2c_ds:send(Uid, Data#{
        <<"gid">> => imboy_hashids:encode(Gid),
        <<"msg_type">> => <<"group_member_alias">>
        }, ToUidLi, save),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


leave(_, _, GMSize, _, _) when GMSize == 0 ->
    ok;
leave(Uid, Gid, _, GM, CurrentUid) ->
    Now = imboy_dt:utc(millisecond),
    Id = maps:get(<<"id">>, GM, 0),
    ToUidLi = group_ds:member_uids(Gid),
    Res = imboy_db:with_transaction(fun(Conn) ->
        Tb2 = group_member_repo:tablename(),
        Sql2 = <<"DELETE FROM ", Tb2/binary, " WHERE id= ", (ec_cnv:to_binary(Id))/binary>>,
        imboy_db:execute(Conn, Sql2, []),

        {ok, Body} = jsone_encode:encode(GM, [native_utf8]),
        Tyep = if
            CurrentUid == Uid ->
                200;
            true ->
                202
        end,
        group_log_repo:add(Conn, #{
            % 日志类型: 100 群转让 101 群解散  200 主动退出群   201 群解散退出群  202 被踢出群
            type => Tyep,
            option_uid => CurrentUid,
            group_id => Gid,
            body => Body,
            created_at => Now
            }),
        Data = #{
            user_id_sum => {raw, <<"user_id_sum-", (ec_cnv:to_binary(Uid))/binary>>},
            member_count => {raw, <<"member_count-1">>},
            updated_at => Now
        },
        imboy_db:update(Conn
            , group_repo:tablename()
            , <<"id = ", (ec_cnv:to_binary(Gid))/binary>>
            , Data
        ),
        Payload = #{
            <<"gid">> => imboy_hashids:encode(Gid),
            <<"msg_type">> => <<"group_member_leave">>
        },
        msg_s2c_ds:send(Uid, Payload, ToUidLi, save),
        group_ds:leave(Uid, Gid),
        ok
    end, #{reraise => true}),
    ?LOG(["leave, ", Uid, Gid, CurrentUid, Res]),
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
