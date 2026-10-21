-module(talk_record_directory_repo).
-moduledoc "个人目录限有效本人或双向有效好友，禁止按全站账号枚举。".
-export([personal_predicate/0, personal_target_tx/3]).

%% Fixed aliases: actor=$1, candidate=u.id. No request text enters SQL syntax.
-spec personal_predicate() -> binary().
personal_predicate() ->
    <<
        "(EXISTS (SELECT 1 FROM \"user\" directory_actor WHERE directory_actor.id=$1"
        " AND directory_actor.status=1 AND directory_actor.account_type=0) AND (u.id=$1"
        " OR (EXISTS (SELECT 1 FROM user_friend f WHERE f.from_user_id=$1"
        " AND f.to_user_id=u.id AND f.status=1)"
        " AND EXISTS (SELECT 1 FROM user_friend r WHERE r.from_user_id=u.id"
        " AND r.to_user_id=$1 AND r.status=1))))"
    >>.

-spec personal_target_tx(any(), integer(), integer()) -> {ok, boolean()} | {error, term()}.
personal_target_tx(C, Actor, Target) when is_integer(Actor), Actor > 0 ->
    Sql =
        <<"SELECT 1 FROM \"user\" u WHERE u.id=$2 AND u.status=1 AND u.account_type=0 AND ",
            (personal_predicate())/binary>>,
    case elib_pg:query(C, Sql, [Actor, Target]) of
        {ok, [_]} -> {ok, true};
        {ok, []} -> {ok, false};
        Error -> Error
    end;
personal_target_tx(_, _, _) ->
    {ok, false}.
