-module(moment_ds).
%%%
% moment domain service
%%%
%% Internal domain service for moment_social. External callers should prefer moment_logic.

-export([create_post/2]).
-export([get_post/1, get_post_any/1]).
-export([delete_post/2]).
-export([delete_post_by_admin/1]).
-export([list_feed_candidates/2]).
-export([list_user_candidates/3]).
-export([can_view_post/2]).
-export([like_post/2, unlike_post/2]).
-export([add_comment/4, list_comments/3, delete_comment/3]).
-export([report_post/4]).
-export([list_post_acl/1]).
-export([list_post_likes/2]).
-export([has_liked/2]).
-export([find_comment_by_id/1]).
-export([page_admin_posts/5]).
-export([list_reports_by_post/2]).
-export([page_admin_reports/3]).
-export([find_report_by_id/1]).
-export([resolve_report/4]).

-include("log.hrl").

-spec create_post(integer(), map()) -> {ok, integer()} | {error, any()}.
create_post(AuthorUid, Data) ->
    Content = maps:get(content, Data, <<>>),
    Media = maps:get(media, Data, []),
    Visibility = maps:get(visibility, Data, 1),
    AllowComment = maps:get(allow_comment, Data, true),
    AllowUids = maps:get(allow_uids, Data, []),
    DenyUids = maps:get(deny_uids, Data, []),
    MediaJson = encode_json(Media, <<"[]">>),
    Now = elib_dt:now(),
    case elib_pg:with_tx(fun(Conn) ->
        PostData = #{
            author_uid => AuthorUid,
            content => Content,
            media => MediaJson,
            visibility => Visibility,
            allow_comment => AllowComment,
            like_count => 0,
            comment_count => 0,
            status => 1,
            created_at => Now,
            updated_at => Now
        },
        case moment_post_repo:add(Conn, PostData) of
            {ok, PostId, _} ->
                case moment_post_acl_repo:replace_for_post(Conn, PostId, AllowUids, DenyUids) of
                    ok ->
                        Recipients = resolve_timeline_recipients(
                            AuthorUid,
                            Visibility,
                            AllowUids,
                            DenyUids
                        ),
                        case moment_timeline_repo:upsert_batch(Conn, Recipients, PostId, AuthorUid, Now) of
                            {ok, _} ->
                                {ok, PostId};
                            {error, Reason2} ->
                                throw({abort_tx, Reason2})
                        end;
                    {error, Reason1} ->
                        throw({abort_tx, Reason1})
                end;
            {error, Reason0} ->
                throw({abort_tx, Reason0})
        end
    end) of
        {ok, PostId} ->
            {ok, PostId};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_post(integer()) -> map() | {error, any()}.
get_post(PostId) ->
    moment_post_repo:find_by_id(PostId).

-spec get_post_any(integer()) -> map() | {error, any()}.
get_post_any(PostId) ->
    moment_post_repo:find_any_by_id(PostId).

-spec delete_post(integer(), integer()) -> ok | {error, any()}.
delete_post(AuthorUid, PostId) ->
    case elib_pg:with_tx(fun(Conn) ->
        case moment_post_repo:soft_delete(PostId, AuthorUid) of
            {ok, Count} when Count > 0 ->
                case moment_timeline_repo:hide_by_post(Conn, PostId) of
                    {ok, _} ->
                        ok;
                    {error, Reason1} ->
                        throw({abort_tx, Reason1})
                end;
            {ok, _} ->
                throw({abort_tx, no_permission});
            {error, Reason0} ->
                throw({abort_tx, Reason0})
        end
    end) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_post_by_admin(integer()) -> ok | {error, any()}.
delete_post_by_admin(PostId) ->
    case elib_pg:with_tx(fun(Conn) ->
        case moment_post_repo:soft_delete_by_admin(PostId) of
            {ok, Count} when Count > 0 ->
                case moment_timeline_repo:hide_by_post(Conn, PostId) of
                    {ok, _} ->
                        ok;
                    {error, Reason1} ->
                        throw({abort_tx, Reason1})
                end;
            {ok, _} ->
                throw({abort_tx, not_found});
            {error, Reason0} ->
                throw({abort_tx, Reason0})
        end
    end) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec list_feed_candidates(integer(), integer()) -> {ok, [map()]} | {error, any()}.
list_feed_candidates(Cursor, Limit) ->
    moment_post_repo:page_feed_candidates(Cursor, Limit).

-spec list_user_candidates(integer(), integer(), integer()) -> {ok, [map()]} | {error, any()}.
list_user_candidates(AuthorUid, Cursor, Limit) ->
    moment_post_repo:page_by_author(AuthorUid, Cursor, Limit).

-spec can_view_post(integer(), map()) -> boolean().
can_view_post(ViewerUid, Post) ->
    Status = maps:get(<<"status">>, Post, 0),
    AuthorUid = maps:get(<<"author_uid">>, Post, 0),
    Visibility = maps:get(<<"visibility">>, Post, 1),
    case Status =:= 1 andalso is_integer(AuthorUid) andalso AuthorUid > 0 of
        false ->
            false;
        true when ViewerUid =:= AuthorUid ->
            true;
        true ->
            case is_denied_between(ViewerUid, AuthorUid) of
                true ->
                    false;
                false ->
                    can_view_by_visibility(ViewerUid, AuthorUid, Visibility, Post)
            end
    end.

-spec like_post(integer(), integer()) -> {ok, changed | noop} | {error, any()}.
like_post(UserId, PostId) ->
    case moment_post_repo:find_by_id(PostId) of
        Post when is_map(Post), map_size(Post) > 0 ->
            case can_view_post(UserId, Post) of
                true ->
                    case elib_pg:with_tx(fun(Conn) ->
                        case moment_like_repo:add(Conn, PostId, UserId) of
                            {ok, true} ->
                                case moment_post_repo:increment_like_count(Conn, PostId, 1) of
                                    {ok, _} -> changed;
                                    {error, Reason1} -> throw({abort_tx, Reason1})
                                end;
                            {ok, false} ->
                                noop;
                            {error, Reason0} ->
                                throw({abort_tx, Reason0})
                        end
                    end) of
                        changed -> {ok, changed};
                        noop -> {ok, noop};
                        {error, Reason2} -> {error, Reason2}
                    end;
                false ->
                    {error, forbidden}
            end;
        _ ->
            {error, not_found}
    end.

-spec unlike_post(integer(), integer()) -> {ok, changed | noop} | {error, any()}.
unlike_post(UserId, PostId) ->
    case moment_post_repo:find_by_id(PostId) of
        Post when is_map(Post), map_size(Post) > 0 ->
            case can_view_post(UserId, Post) of
                true ->
                    case elib_pg:with_tx(fun(Conn) ->
                        case moment_like_repo:remove(Conn, PostId, UserId) of
                            {ok, true} ->
                                case moment_post_repo:increment_like_count(Conn, PostId, -1) of
                                    {ok, _} -> changed;
                                    {error, Reason1} -> throw({abort_tx, Reason1})
                                end;
                            {ok, false} ->
                                noop;
                            {error, Reason0} ->
                                throw({abort_tx, Reason0})
                        end
                    end) of
                        changed -> {ok, changed};
                        noop -> {ok, noop};
                        {error, Reason2} -> {error, Reason2}
                    end;
                false ->
                    {error, forbidden}
            end;
        _ ->
            {error, not_found}
    end.

-spec add_comment(integer(), integer(), binary(), integer()) -> {ok, integer()} | {error, any()}.
add_comment(UserId, PostId, Content, ReplyToUid0) ->
    case moment_post_repo:find_by_id(PostId) of
        Post when is_map(Post), map_size(Post) > 0 ->
            AllowComment = maps:get(<<"allow_comment">>, Post, true),
            case AllowComment of
                false ->
                    {error, comment_closed};
                true ->
                    case can_view_post(UserId, Post) of
                        false ->
                            {error, forbidden};
                        true ->
                            ReplyToUid = normalize_optional_uid(ReplyToUid0),
                            Now = elib_dt:now(),
                            case elib_pg:with_tx(fun(Conn) ->
                                Data = #{
                                    post_id => PostId,
                                    user_id => UserId,
                                    reply_to_uid => ReplyToUid,
                                    content => Content,
                                    status => 1,
                                    created_at => Now,
                                    updated_at => Now
                                },
                                case moment_comment_repo:add(Conn, Data) of
                                    {ok, CommentId, _} ->
                                        case moment_post_repo:increment_comment_count(Conn, PostId, 1) of
                                            {ok, _} ->
                                                {ok, CommentId};
                                            {error, Reason1} ->
                                                throw({abort_tx, Reason1})
                                        end;
                                    {error, Reason0} ->
                                        throw({abort_tx, Reason0})
                                end
                            end) of
                                {ok, CommentId} ->
                                    {ok, CommentId};
                                {error, Reason2} ->
                                    {error, Reason2}
                            end
                    end
            end;
        _ ->
            {error, not_found}
    end.

-spec list_comments(integer(), integer(), integer()) -> {ok, [map()]} | {error, any()}.
list_comments(PostId, Cursor, Limit) ->
    moment_comment_repo:page_by_post(PostId, Cursor, Limit).

-spec delete_comment(integer(), integer(), integer()) -> ok | {error, any()}.
delete_comment(UserId, PostId, CommentId) ->
    Comment = moment_comment_repo:find_any_by_id(CommentId),
    Post = moment_post_repo:find_any_by_id(PostId),
    case {Comment, Post} of
        {CMap, PMap} when is_map(CMap), is_map(PMap), map_size(CMap) > 0, map_size(PMap) > 0 ->
            CommentPostId = maps:get(<<"post_id">>, CMap, 0),
            CommentOwner = maps:get(<<"user_id">>, CMap, 0),
            PostOwner = maps:get(<<"author_uid">>, PMap, 0),
            CommentStatus = maps:get(<<"status">>, CMap, 0),
            case CommentPostId =:= PostId andalso CommentStatus =:= 1 of
                false ->
                    {error, not_found};
                true ->
                    case UserId =:= CommentOwner orelse UserId =:= PostOwner of
                        false ->
                            {error, no_permission};
                        true ->
                            case elib_pg:with_tx(fun(Conn) ->
                                case moment_comment_repo:soft_delete(Conn, CommentId) of
                                    {ok, Count} when Count > 0 ->
                                        case moment_post_repo:increment_comment_count(Conn, PostId, -1) of
                                            {ok, _} -> ok;
                                            {error, Reason1} -> throw({abort_tx, Reason1})
                                        end;
                                    {ok, _} ->
                                        ok;
                                    {error, Reason0} ->
                                        throw({abort_tx, Reason0})
                                end
                            end) of
                                ok ->
                                    ok;
                                {error, Reason2} ->
                                    {error, Reason2}
                            end
                    end
            end;
        _ ->
            {error, not_found}
    end.

-spec report_post(integer(), integer(), binary(), binary()) ->
    {ok, integer()} | {error, any()}.
report_post(UserId, PostId, Reason, Desc) ->
    case moment_post_repo:find_by_id(PostId) of
        Post when is_map(Post), map_size(Post) > 0 ->
            AuthorUid = maps:get(<<"author_uid">>, Post, 0),
            case UserId =:= AuthorUid of
                true ->
                    {error, invalid_reporter};
                false ->
                    moment_report_repo:upsert(PostId, UserId, Reason, Desc)
            end;
        _ ->
            {error, not_found}
    end.

-spec list_post_acl(integer()) -> {[integer()], [integer()]}.
list_post_acl(PostId) ->
    Key = {moment_acl, PostId},
    Fun = fun() ->
        {
            moment_post_acl_repo:list_uids_by_post(PostId, 1),
            moment_post_acl_repo:list_uids_by_post(PostId, 2)
        }
    end,
    imboy_cache:memo(Fun, Key, 300).

-spec list_post_likes(integer(), integer()) -> {ok, [map()]} | {error, any()}.
list_post_likes(PostId, Limit) ->
    moment_like_repo:list_by_post(PostId, Limit).

-spec can_view_by_visibility(integer(), integer(), integer(), map()) -> boolean().
can_view_by_visibility(ViewerUid, AuthorUid, 0, _Post) ->
    ViewerUid > 0 andalso AuthorUid > 0;
can_view_by_visibility(ViewerUid, AuthorUid, 1, _Post) ->
    friend_ds:is_friend(AuthorUid, ViewerUid);
can_view_by_visibility(_ViewerUid, _AuthorUid, 2, _Post) ->
    false;
can_view_by_visibility(ViewerUid, _AuthorUid, 3, Post) ->
    PostId = maps:get(<<"id">>, Post, 0),
    {AllowUids, _} = list_post_acl(PostId),
    lists:member(ViewerUid, AllowUids);
can_view_by_visibility(ViewerUid, AuthorUid, 4, Post) ->
    case friend_ds:is_friend(AuthorUid, ViewerUid) of
        false ->
            false;
        true ->
            PostId = maps:get(<<"id">>, Post, 0),
            {_, DenyUids} = list_post_acl(PostId),
            not lists:member(ViewerUid, DenyUids)
    end;
can_view_by_visibility(_ViewerUid, _AuthorUid, _Visibility, _Post) ->
    false.

-spec resolve_timeline_recipients(integer(), integer(), [integer()], [integer()]) -> [integer()].
resolve_timeline_recipients(AuthorUid, Visibility, AllowUids0, DenyUids0) ->
    Friends = normalize_uid_list(friend_ds:list_by_uid(AuthorUid)),
    AllowUids = normalize_uid_list(AllowUids0),
    DenyUids = normalize_uid_list(DenyUids0),
    Base = case Visibility of
        2 -> [AuthorUid];
        3 -> [AuthorUid | AllowUids];
        4 -> [AuthorUid | [Uid || Uid <- Friends, not lists:member(Uid, DenyUids)]];
        _ -> [AuthorUid | Friends]
    end,
    lists:usort([Uid || Uid <- Base, Uid > 0, not is_denied_between(AuthorUid, Uid)]).

-spec normalize_uid_list(term()) -> [integer()].
normalize_uid_list(Uids) when is_list(Uids) ->
    lists:usort([Uid || Uid <- Uids, is_integer(Uid), Uid > 0]);
normalize_uid_list(_) ->
    [].

-spec normalize_optional_uid(term()) -> integer() | undefined.
normalize_optional_uid(undefined) ->
    undefined;
normalize_optional_uid(0) ->
    undefined;
normalize_optional_uid(Uid) when is_integer(Uid), Uid > 0 ->
    Uid;
normalize_optional_uid(_) ->
    undefined.

-spec is_denied_between(integer(), integer()) -> boolean().
is_denied_between(UidA, UidB) when UidA =< 0; UidB =< 0 ->
    true;
is_denied_between(UidA, UidB) ->
    user_denylist_repo:in_denylist(UidA, UidB) > 0
        orelse user_denylist_repo:in_denylist(UidB, UidA) > 0.

-spec encode_json(term(), binary()) -> binary().
encode_json(Value, Default) ->
    try jsone:encode(Value, [native_utf8]) of
        Bin when is_binary(Bin) -> Bin;
        _ -> Default
    catch
        _:_ ->
            Default
    end.

%% G3: moment_logic 不应直调 moment_*_repo / thin DS wrappers

-spec has_liked(integer(), integer()) -> boolean().
has_liked(PostId, Uid) -> moment_like_repo:has_liked(PostId, Uid).

-spec find_comment_by_id(integer()) -> map() | {error, any()}.
find_comment_by_id(CommentId) -> moment_comment_repo:find_by_id(CommentId).

-spec page_admin_posts(binary() | undefined, integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, any()}.
page_admin_posts(Keyword, Uid, Status, Page, Size) ->
    moment_post_repo:page_admin(Keyword, Uid, Status, Page, Size).

-spec list_reports_by_post(integer(), integer()) -> {ok, [map()]} | {error, any()}.
list_reports_by_post(PostId, Limit) -> moment_report_repo:list_by_post(PostId, Limit).

-spec page_admin_reports(integer(), integer(), integer()) -> {ok, map()} | {error, any()}.
page_admin_reports(Status, Page, Size) -> moment_report_repo:page_admin(Status, Page, Size).

-spec find_report_by_id(integer()) -> map() | {error, any()}.
find_report_by_id(ReportId) -> moment_report_repo:find_by_id(ReportId).

-spec resolve_report(integer(), integer(), binary() | undefined, integer()) -> ok | {error, any()}.
resolve_report(ReportId, Result, Note, AdmUid) ->
    moment_report_repo:resolve(ReportId, Result, Note, AdmUid).
