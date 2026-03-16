-module(moment_logic_notify).
%%%
% moment notification helpers
%%%
%% Internal cross-cutting helper. Keep notification glue behind moment_logic.

-export([notify_post_created/2]).
-export([notify_post_deleted/2]).
-export([notify_post_deleted/3]).
-export([notify_post_liked/3]).
-export([notify_post_commented/4]).

-include("log.hrl").

-spec notify_post_created(integer(), integer()) -> ok.
notify_post_created(AuthorUid, PostId) ->
    RecipientUids = resolve_post_recipients(AuthorUid),
    Payload = #{
        <<"moment_id">> => elib_hashids:encode(PostId),
        <<"author_uid">> => elib_hashids:encode(AuthorUid)
    },
    safe_send(RecipientUids, <<"moment_new">>, Payload, save).

-spec notify_post_deleted(integer(), integer()) -> ok.
notify_post_deleted(AuthorUid, PostId) ->
    notify_post_deleted(AuthorUid, PostId, AuthorUid).

-spec notify_post_deleted(integer(), integer(), integer()) -> ok.
notify_post_deleted(AuthorUid, PostId, DeletedByUid) ->
    RecipientUids = resolve_post_recipients(AuthorUid),
    Payload = #{
        <<"moment_id">> => elib_hashids:encode(PostId),
        <<"deleted_by">> => elib_hashids:encode(DeletedByUid)
    },
    safe_send(RecipientUids, <<"moment_deleted">>, Payload, save).

-spec notify_post_liked(integer(), integer(), integer()) -> ok.
notify_post_liked(FromUid, PostId, AuthorUid) ->
    case FromUid =:= AuthorUid of
        true ->
            ok;
        false ->
            Payload = #{
                <<"moment_id">> => elib_hashids:encode(PostId),
                <<"from_uid">> => elib_hashids:encode(FromUid)
            },
            safe_send([AuthorUid], <<"moment_like">>, Payload, no_save)
    end.

-spec notify_post_commented(integer(), integer(), integer(), integer()) -> ok.
notify_post_commented(FromUid, PostId, CommentId, AuthorUid) ->
    case FromUid =:= AuthorUid of
        true ->
            ok;
        false ->
            Payload = #{
                <<"moment_id">> => elib_hashids:encode(PostId),
                <<"comment_id">> => elib_hashids:encode(CommentId),
                <<"from_uid">> => elib_hashids:encode(FromUid)
            },
            safe_send([AuthorUid], <<"moment_comment">>, Payload, save)
    end.

-spec resolve_post_recipients(integer()) -> [integer()].
resolve_post_recipients(AuthorUid) ->
    Friends = case catch friend_ds:list_by_uid(AuthorUid) of
        List when is_list(List) ->
            [Uid || Uid <- List, is_integer(Uid), Uid > 0];
        _ ->
            []
    end,
    lists:usort([AuthorUid | Friends]).

-spec safe_send([integer()], binary(), map(), atom()) -> ok.
safe_send([], _Action, _Payload, _Mode) ->
    ok;
safe_send(ToUids, Action, Payload, Mode) ->
    try
        _ = msg_s2c_ds:send(0, ToUids, Action, <<>>, null, Payload, Mode),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?DEBUG_LOG("moment s2c notify failed: ~p", [{Class, Reason, Stacktrace}]),
            ok
    end.
