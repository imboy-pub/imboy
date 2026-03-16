-module(moment_logic).
%%%
% moment business logic
%%%
%% Stable domain boundary for moment_social.
%% API, admin, and report adapters should call this module instead of ds/repo internals.

-export([create_post/2]).
-export([get_post/2]).
-export([delete_post/2]).
-export([feed/3]).
-export([user_posts/4]).
-export([like_post/2, unlike_post/2]).
-export([add_comment/4]).
-export([list_comments/4]).
-export([delete_comment/3]).
-export([report_post/4]).

-export([admin_list_posts/5]).
-export([admin_post_detail/1]).
-export([admin_delete_post/3]).
-export([admin_list_reports/3]).
-export([admin_resolve_report/4]).

-include("log.hrl").

-define(ADM_MOMENT_AUDIT_TYPE, 903).

-spec create_post(integer(), map()) -> {ok, map()} | {error, binary()}.
create_post(Uid, PostVals) ->
    Content = normalize_non_empty_binary(maps:get(<<"content">>, PostVals, <<>>)),
    Media = parse_media(maps:get(<<"media">>, PostVals, [])),
    Visibility = ec_cnv:to_integer(maps:get(<<"visibility">>, PostVals, 1)),
    AllowComment = parse_bool(maps:get(<<"allow_comment">>, PostVals, true), true),
    AllowUids = parse_uid_list(maps:get(<<"allow_uids">>, PostVals, [])),
    DenyUids = parse_uid_list(maps:get(<<"deny_uids">>, PostVals, [])),

    case validate_create(Content, Media, Visibility) of
        ok ->
            Data = #{
                content => Content,
                media => Media,
                visibility => Visibility,
                allow_comment => AllowComment,
                allow_uids => AllowUids,
                deny_uids => DenyUids
            },
            case moment_ds:create_post(Uid, Data) of
                {ok, PostId} ->
                    Post = moment_ds:get_post(PostId),
                    Payload = post_transfer(Post),
                    _ = moment_logic_notify:notify_post_created(Uid, PostId),
                    {ok, Payload};
                {error, Reason} ->
                    {error, normalize_error(Reason, <<"发布动态失败"/utf8>>)}
            end;
        {error, Msg} ->
            {error, Msg}
    end.

-spec get_post(integer(), term()) -> {ok, map()} | {error, binary()}.
get_post(Uid, PostIdRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    case PostId > 0 of
        false ->
            {error, <<"动态不存在"/utf8>>};
        true ->
            case moment_ds:get_post(PostId) of
                Post when is_map(Post), map_size(Post) > 0 ->
                    case moment_ds:can_view_post(Uid, Post) of
                        true ->
                            Liked = moment_like_repo:has_liked(PostId, Uid),
                            Payload = post_transfer(Post),
                            {ok, Payload#{<<"liked">> => Liked}};
                        false ->
                            {error, <<"无权限查看该动态"/utf8>>}
                    end;
                _ ->
                    {error, <<"动态不存在"/utf8>>}
            end
    end.

-spec delete_post(integer(), term()) -> ok | {error, binary()}.
delete_post(Uid, PostIdRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    case PostId > 0 of
        false ->
            {error, <<"动态不存在"/utf8>>};
        true ->
            case moment_ds:delete_post(Uid, PostId) of
                ok ->
                    _ = moment_logic_notify:notify_post_deleted(Uid, PostId, Uid),
                    ok;
                {error, no_permission} ->
                    {error, <<"无权限删除该动态"/utf8>>};
                {error, not_found} ->
                    {error, <<"动态不存在"/utf8>>};
                {error, Reason} ->
                    {error, normalize_error(Reason, <<"删除动态失败"/utf8>>)}
            end
    end.

-spec feed(integer(), integer(), integer()) -> {ok, map()} | {error, binary()}.
feed(Uid, Cursor, Limit) ->
    Limit2 = clamp(Limit, 1, 100),
    CandidateLimit = erlang:min(Limit2 * 5, 500),
    case moment_ds:list_feed_candidates(Cursor, CandidateLimit) of
        {ok, Posts} ->
            VisiblePosts = take_visible_posts(Uid, Posts, Limit2),
            List = [post_transfer(P) || P <- VisiblePosts],
            NextCursor = next_cursor_from_posts(VisiblePosts),
            {ok, #{list => List, cursor => NextCursor, limit => Limit2}};
        {error, Reason} ->
            {error, normalize_error(Reason, <<"查询失败"/utf8>>)}
    end.

-spec user_posts(integer(), term(), integer(), integer()) -> {ok, map()} | {error, binary()}.
user_posts(Uid, TargetUidRaw, Cursor, Limit) ->
    TargetUid = decode_positive_id(TargetUidRaw),
    case TargetUid > 0 of
        false ->
            {error, <<"用户不存在"/utf8>>};
        true ->
            Limit2 = clamp(Limit, 1, 100),
            CandidateLimit = erlang:min(Limit2 * 5, 500),
            case moment_ds:list_user_candidates(TargetUid, Cursor, CandidateLimit) of
                {ok, Posts} ->
                    VisiblePosts = take_visible_posts(Uid, Posts, Limit2),
                    List = [post_transfer(P) || P <- VisiblePosts],
                    NextCursor = next_cursor_from_posts(VisiblePosts),
                    {ok, #{list => List, cursor => NextCursor, limit => Limit2}};
                {error, Reason} ->
                    {error, normalize_error(Reason, <<"查询失败"/utf8>>)}
            end
    end.

-spec like_post(integer(), term()) -> ok | {error, binary()}.
like_post(Uid, PostIdRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    case PostId > 0 of
        false ->
            {error, <<"动态不存在"/utf8>>};
        true ->
            case moment_ds:like_post(Uid, PostId) of
                {ok, changed} ->
                    Post = moment_ds:get_post(PostId),
                    AuthorUid = maps:get(<<"author_uid">>, Post, 0),
                    _ = moment_logic_notify:notify_post_liked(Uid, PostId, AuthorUid),
                    ok;
                {ok, noop} ->
                    ok;
                {error, forbidden} ->
                    {error, <<"无权限查看该动态"/utf8>>};
                {error, not_found} ->
                    {error, <<"动态不存在"/utf8>>};
                {error, Reason} ->
                    {error, normalize_error(Reason, <<"点赞失败"/utf8>>)}
            end
    end.

-spec unlike_post(integer(), term()) -> ok | {error, binary()}.
unlike_post(Uid, PostIdRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    case PostId > 0 of
        false ->
            {error, <<"动态不存在"/utf8>>};
        true ->
            case moment_ds:unlike_post(Uid, PostId) of
                {ok, _} ->
                    ok;
                {error, forbidden} ->
                    {error, <<"无权限查看该动态"/utf8>>};
                {error, not_found} ->
                    {error, <<"动态不存在"/utf8>>};
                {error, Reason} ->
                    {error, normalize_error(Reason, <<"取消点赞失败"/utf8>>)}
            end
    end.

-spec add_comment(integer(), term(), binary(), term()) -> {ok, map()} | {error, binary()}.
add_comment(Uid, PostIdRaw, ContentRaw, ReplyToUidRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    Content = normalize_non_empty_binary(ContentRaw),
    ReplyToUid = decode_optional_positive_id(ReplyToUidRaw),
    case validate_comment(Content, PostId) of
        ok ->
            case moment_ds:add_comment(Uid, PostId, Content, ReplyToUid) of
                {ok, CommentId} ->
                    Comment = moment_comment_repo:find_by_id(CommentId),
                    Post = moment_ds:get_post(PostId),
                    AuthorUid = maps:get(<<"author_uid">>, Post, 0),
                    _ = moment_logic_notify:notify_post_commented(Uid, PostId, CommentId, AuthorUid),
                    {ok, comment_transfer(Comment)};
                {error, forbidden} ->
                    {error, <<"无权限查看该动态"/utf8>>};
                {error, not_found} ->
                    {error, <<"动态不存在"/utf8>>};
                {error, comment_closed} ->
                    {error, <<"评论已关闭"/utf8>>};
                {error, Reason} ->
                    {error, normalize_error(Reason, <<"评论失败"/utf8>>)}
            end;
        {error, Msg} ->
            {error, Msg}
    end.

-spec list_comments(integer(), term(), integer(), integer()) -> {ok, map()} | {error, binary()}.
list_comments(Uid, PostIdRaw, Cursor, Limit) ->
    PostId = decode_positive_id(PostIdRaw),
    case PostId > 0 of
        false ->
            {error, <<"动态不存在"/utf8>>};
        true ->
            case moment_ds:get_post(PostId) of
                Post when is_map(Post), map_size(Post) > 0 ->
                    case moment_ds:can_view_post(Uid, Post) of
                        false ->
                            {error, <<"无权限查看该动态"/utf8>>};
                        true ->
                            Limit2 = clamp(Limit, 1, 100),
                            case moment_ds:list_comments(PostId, Cursor, Limit2) of
                                {ok, Comments} ->
                                    List = [comment_transfer(C) || C <- Comments],
                                    NextCursor = next_cursor_from_comments(Comments),
                                    {ok, #{list => List, cursor => NextCursor, limit => Limit2}};
                                {error, Reason} ->
                                    {error, normalize_error(Reason, <<"查询失败"/utf8>>)}
                            end
                    end;
                _ ->
                    {error, <<"动态不存在"/utf8>>}
            end
    end.

-spec delete_comment(integer(), term(), term()) -> ok | {error, binary()}.
delete_comment(Uid, PostIdRaw, CommentIdRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    CommentId = decode_positive_id(CommentIdRaw),
    case PostId > 0 andalso CommentId > 0 of
        false ->
            {error, <<"评论不存在"/utf8>>};
        true ->
            case moment_ds:delete_comment(Uid, PostId, CommentId) of
                ok ->
                    ok;
                {error, no_permission} ->
                    {error, <<"无权限删除该评论"/utf8>>};
                {error, not_found} ->
                    {error, <<"评论不存在"/utf8>>};
                {error, Reason} ->
                    {error, normalize_error(Reason, <<"删除评论失败"/utf8>>)}
            end
    end.

-spec report_post(integer(), term(), binary(), binary()) -> {ok, map()} | {error, binary()}.
report_post(Uid, PostIdRaw, ReasonRaw, DescRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    Reason = normalize_non_empty_binary(ReasonRaw),
    Desc = normalize_non_empty_binary(DescRaw),
    case PostId > 0 andalso Reason =/= <<>> of
        false ->
            {error, <<"举报参数无效"/utf8>>};
        true ->
            case moment_ds:report_post(Uid, PostId, Reason, Desc) of
                {ok, ReportId} ->
                    {ok, #{<<"report_id">> => elib_hashids:encode(ReportId)}};
                {error, invalid_reporter} ->
                    {error, <<"不能举报自己的动态"/utf8>>};
                {error, not_found} ->
                    {error, <<"动态不存在"/utf8>>};
                {error, Reason2} ->
                    {error, normalize_error(Reason2, <<"举报失败"/utf8>>)}
            end
    end.

-spec admin_list_posts(binary() | undefined, term(), integer(), integer(), integer()) ->
    {ok, map()} | {error, binary()}.
admin_list_posts(Keyword, UidRaw, Status, Page, Size) ->
    Uid = decode_optional_positive_id(UidRaw),
    Page2 = clamp(Page, 1, 1000000),
    Size2 = clamp(Size, 1, 100),
    case moment_post_repo:page_admin(Keyword, Uid, Status, Page2, Size2) of
        {ok, Payload} ->
            List = maps:get(list, Payload, []),
            List2 = [admin_post_transfer(Post) || Post <- List],
            {ok, Payload#{list => List2}};
        {error, Reason} ->
            {error, normalize_error(Reason, <<"查询失败"/utf8>>)}
    end.

-spec admin_post_detail(term()) -> {ok, map()} | {error, binary()}.
admin_post_detail(PostIdRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    case PostId > 0 of
        false ->
            {error, <<"动态不存在"/utf8>>};
        true ->
            case moment_ds:get_post_any(PostId) of
                Post when is_map(Post), map_size(Post) > 0 ->
                    {AllowUids, DenyUids} = moment_ds:list_post_acl(PostId),
                    Reports = case moment_report_repo:list_by_post(PostId, 50) of
                        {ok, Rows} -> [report_transfer(R) || R <- Rows];
                        _ -> []
                    end,
                    PostPayload = admin_post_transfer(Post),
                    Payload = PostPayload#{
                        <<"acl">> => #{
                            <<"allow_uids">> => [elib_hashids:encode(U) || U <- AllowUids],
                            <<"deny_uids">> => [elib_hashids:encode(U) || U <- DenyUids]
                        },
                        <<"reports">> => Reports
                    },
                    {ok, Payload};
                _ ->
                    {error, <<"动态不存在"/utf8>>}
            end
    end.

-spec admin_delete_post(integer(), term(), binary()) -> ok | {error, binary()}.
admin_delete_post(AdmUid, PostIdRaw, ReasonRaw) ->
    PostId = decode_positive_id(PostIdRaw),
    Reason = normalize_non_empty_binary(ReasonRaw),
    case PostId > 0 of
        false ->
            {error, <<"动态不存在"/utf8>>};
        true ->
            Post = moment_ds:get_post_any(PostId),
            AuthorUid = maps:get(<<"author_uid">>, Post, 0),
            case moment_ds:delete_post_by_admin(PostId) of
                ok ->
                    _ = audit_admin_action(
                        AdmUid,
                        <<"delete_moment">>,
                        PostId,
                        #{<<"reason">> => Reason}
                    ),
                    _ = moment_logic_notify:notify_post_deleted(AuthorUid, PostId, AdmUid),
                    ok;
                {error, not_found} ->
                    {error, <<"动态不存在"/utf8>>};
                {error, Reason2} ->
                    {error, normalize_error(Reason2, <<"删除失败"/utf8>>)}
            end
    end.

-spec admin_list_reports(integer(), integer(), integer()) -> {ok, map()} | {error, binary()}.
admin_list_reports(Status, Page, Size) ->
    Page2 = clamp(Page, 1, 1000000),
    Size2 = clamp(Size, 1, 100),
    case moment_report_repo:page_admin(Status, Page2, Size2) of
        {ok, Payload} ->
            List = maps:get(list, Payload, []),
            List2 = [report_transfer(Item) || Item <- List],
            {ok, Payload#{list => List2}};
        {error, Reason} ->
            {error, normalize_error(Reason, <<"查询失败"/utf8>>)}
    end.

-spec admin_resolve_report(integer(), term(), integer(), binary()) -> ok | {error, binary()}.
admin_resolve_report(AdmUid, ReportIdRaw, Result, NoteRaw) ->
    ReportId = decode_positive_id(ReportIdRaw),
    Note = normalize_non_empty_binary(NoteRaw),
    case ReportId > 0 andalso (Result =:= 1 orelse Result =:= 2) of
        false ->
            {error, <<"举报参数无效"/utf8>>};
        true ->
            case moment_report_repo:find_by_id(ReportId) of
                Report when is_map(Report), map_size(Report) > 0 ->
                    PostId = maps:get(<<"post_id">>, Report, 0),
                    case moment_report_repo:resolve(ReportId, Result, Note, AdmUid) of
                        {ok, _} ->
                            _ = maybe_delete_post_on_violation(AdmUid, PostId, Result),
                            _ = audit_admin_action(
                                AdmUid,
                                <<"resolve_moment_report">>,
                                ReportId,
                                #{
                                    <<"post_id">> => PostId,
                                    <<"result">> => Result,
                                    <<"note">> => Note
                                }
                            ),
                            ok;
                        {error, Reason} ->
                            {error, normalize_error(Reason, <<"处理举报失败"/utf8>>)}
                    end;
                _ ->
                    {error, <<"举报记录不存在"/utf8>>}
            end
    end.

-spec maybe_delete_post_on_violation(integer(), integer(), integer()) -> ok.
maybe_delete_post_on_violation(_AdmUid, _PostId, Result) when Result =/= 2 ->
    ok;
maybe_delete_post_on_violation(AdmUid, PostId, 2) ->
    Post = moment_ds:get_post_any(PostId),
    AuthorUid = maps:get(<<"author_uid">>, Post, 0),
    case moment_ds:delete_post_by_admin(PostId) of
        ok ->
            _ = moment_logic_notify:notify_post_deleted(AuthorUid, PostId, AdmUid),
            ok;
        _ ->
            ok
    end.

-spec post_transfer(map()) -> map().
post_transfer(Post) ->
    PostId = maps:get(<<"id">>, Post, 0),
    AuthorUid = maps:get(<<"author_uid">>, Post, 0),
    MediaRaw = maps:get(<<"media">>, Post, <<"[]">>),
    Media = decode_json(MediaRaw, []),
    LikeCount = maps:get(<<"like_count">>, Post, 0),
    CommentCount = maps:get(<<"comment_count">>, Post, 0),
    Post2 = Post#{
        <<"id">> => safe_encode(PostId),
        <<"author_uid">> => safe_encode(AuthorUid),
        <<"media">> => Media,
        <<"stats">> => #{
            <<"like_count">> => LikeCount,
            <<"comment_count">> => CommentCount
        }
    },
    maps:without([<<"like_count">>, <<"comment_count">>], Post2).

-spec admin_post_transfer(map()) -> map().
admin_post_transfer(Post) ->
    post_transfer(Post).

-spec comment_transfer(map()) -> map().
comment_transfer(Comment) ->
    CommentId = maps:get(<<"id">>, Comment, 0),
    PostId = maps:get(<<"post_id">>, Comment, 0),
    UserId = maps:get(<<"user_id">>, Comment, 0),
    ReplyToUid0 = maps:get(<<"reply_to_uid">>, Comment, 0),
    ReplyToUid = case ReplyToUid0 of
        Uid when is_integer(Uid), Uid > 0 -> safe_encode(Uid);
        _ -> <<>>
    end,
    #{
        <<"id">> => safe_encode(CommentId),
        <<"moment_id">> => safe_encode(PostId),
        <<"user_id">> => safe_encode(UserId),
        <<"reply_to_uid">> => ReplyToUid,
        <<"content">> => maps:get(<<"content">>, Comment, <<>>),
        <<"created_at">> => maps:get(<<"created_at">>, Comment, <<>>),
        <<"updated_at">> => maps:get(<<"updated_at">>, Comment, <<>>)
    }.

-spec report_transfer(map()) -> map().
report_transfer(Report) ->
    ReportId = maps:get(<<"id">>, Report, 0),
    PostId = maps:get(<<"post_id">>, Report, 0),
    ReporterUid = maps:get(<<"reporter_uid">>, Report, 0),
    HandledBy0 = maps:get(<<"handled_by">>, Report, 0),
    HandledBy = case HandledBy0 of
        Uid when is_integer(Uid), Uid > 0 -> safe_encode(Uid);
        _ -> <<>>
    end,
    Report#{
        <<"id">> => safe_encode(ReportId),
        <<"post_id">> => safe_encode(PostId),
        <<"reporter_uid">> => safe_encode(ReporterUid),
        <<"handled_by">> => HandledBy
    }.

-spec validate_create(binary(), list(), integer()) -> ok | {error, binary()}.
validate_create(Content, Media, _Visibility) when Content =:= <<>>, Media =:= [] ->
    {error, <<"动态内容不能为空"/utf8>>};
validate_create(Content, _Media, _Visibility) when byte_size(Content) > 5000 ->
    {error, <<"内容长度不能超过5000"/utf8>>};
validate_create(_Content, Media, _Visibility) when length(Media) > 9 ->
    {error, <<"媒体数量不能超过9个"/utf8>>};
validate_create(_Content, _Media, Visibility)
    when Visibility < 0; Visibility > 4 ->
    {error, <<"可见性参数无效"/utf8>>};
validate_create(_Content, _Media, _Visibility) ->
    ok.

-spec validate_comment(binary(), integer()) -> ok | {error, binary()}.
validate_comment(_Content, PostId) when PostId =< 0 ->
    {error, <<"动态不存在"/utf8>>};
validate_comment(Content, _PostId) when Content =:= <<>> ->
    {error, <<"评论内容不能为空"/utf8>>};
validate_comment(Content, _PostId) when byte_size(Content) > 500 ->
    {error, <<"评论内容不能超过500字"/utf8>>};
validate_comment(_Content, _PostId) ->
    ok.

-spec normalize_non_empty_binary(term()) -> binary().
normalize_non_empty_binary(undefined) ->
    <<>>;
normalize_non_empty_binary(Value) when is_binary(Value) ->
    list_to_binary(string:trim(binary_to_list(Value)));
normalize_non_empty_binary(Value) when is_list(Value) ->
    list_to_binary(string:trim(Value));
normalize_non_empty_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_non_empty_binary(_) ->
    <<>>.

-spec parse_media(term()) -> list().
parse_media(Media) when is_list(Media) ->
    [Item || Item <- Media, is_map(Item)];
parse_media(Media) when is_binary(Media), Media =/= <<>> ->
    case decode_json(Media, []) of
        List when is_list(List) -> [Item || Item <- List, is_map(Item)];
        _ -> []
    end;
parse_media(Media) when is_map(Media) ->
    [Media];
parse_media(_) ->
    [].

-spec parse_uid_list(term()) -> [integer()].
parse_uid_list(undefined) ->
    [];
parse_uid_list(Uids) when is_list(Uids) ->
    lists:usort([Id || Id <- [decode_positive_id(U) || U <- Uids], Id > 0]);
parse_uid_list(Uids) when is_binary(Uids), Uids =/= <<>> ->
    case decode_json(Uids, []) of
        List when is_list(List) ->
            parse_uid_list(List);
        _ ->
            []
    end;
parse_uid_list(_) ->
    [].

-spec parse_bool(term(), boolean()) -> boolean().
parse_bool(true, _Default) ->
    true;
parse_bool(false, _Default) ->
    false;
parse_bool(1, _Default) ->
    true;
parse_bool(0, _Default) ->
    false;
parse_bool(<<"1">>, _Default) ->
    true;
parse_bool(<<"0">>, _Default) ->
    false;
parse_bool(<<"true">>, _Default) ->
    true;
parse_bool(<<"false">>, _Default) ->
    false;
parse_bool(_, Default) ->
    Default.

-spec take_visible_posts(integer(), [map()], integer()) -> [map()].
take_visible_posts(Uid, Posts, Limit) ->
    take_visible_posts(Uid, Posts, Limit, []).

-spec take_visible_posts(integer(), [map()], integer(), [map()]) -> [map()].
take_visible_posts(_Uid, _Posts, Limit, Acc) when length(Acc) >= Limit ->
    lists:reverse(Acc);
take_visible_posts(_Uid, [], _Limit, Acc) ->
    lists:reverse(Acc);
take_visible_posts(Uid, [Post | Rest], Limit, Acc) ->
    case moment_ds:can_view_post(Uid, Post) of
        true ->
            take_visible_posts(Uid, Rest, Limit, [Post | Acc]);
        false ->
            take_visible_posts(Uid, Rest, Limit, Acc)
    end.

-spec next_cursor_from_posts([map()]) -> binary().
next_cursor_from_posts([]) ->
    <<>>;
next_cursor_from_posts(Posts) ->
    Last = lists:last(Posts),
    safe_encode(maps:get(<<"id">>, Last, 0)).

-spec next_cursor_from_comments([map()]) -> binary().
next_cursor_from_comments([]) ->
    <<>>;
next_cursor_from_comments(Comments) ->
    Last = lists:last(Comments),
    safe_encode(maps:get(<<"id">>, Last, 0)).

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) when is_integer(Value), Value > 0 ->
    Value;
decode_positive_id(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            Int = ec_cnv:to_integer(Value),
            case Int > 0 of true -> Int; false -> 0 end;
        false ->
            case catch elib_hashids:decode(Value) of
                Id when is_integer(Id), Id > 0 -> Id;
                _ -> 0
            end
    end;
decode_positive_id(Value) when is_list(Value) ->
    decode_positive_id(ec_cnv:to_binary(Value));
decode_positive_id(_) ->
    0.

-spec decode_optional_positive_id(term()) -> integer().
decode_optional_positive_id(undefined) ->
    0;
decode_optional_positive_id(<<>>) ->
    0;
decode_optional_positive_id(Value) ->
    decode_positive_id(Value).

-spec safe_encode(term()) -> binary() | term().
safe_encode(Id) when is_integer(Id), Id > 0 ->
    elib_hashids:encode(Id);
safe_encode(Id) ->
    Id.

-spec normalize_error(term(), binary()) -> binary().
normalize_error(Msg, _Default) when is_binary(Msg) ->
    Msg;
normalize_error(Msg, _Default) when is_list(Msg) ->
    unicode:characters_to_binary(Msg);
normalize_error(_Msg, Default) ->
    Default.

-spec decode_json(term(), term()) -> term().
decode_json(Value, Default) when is_binary(Value), Value =/= <<>> ->
    try jsone:decode(Value, [{object_format, map}]) of
        Decoded -> Decoded
    catch
        _:_ -> Default
    end;
decode_json(Value, _Default) when is_list(Value); is_map(Value) ->
    Value;
decode_json(_Value, Default) ->
    Default.

-spec clamp(integer(), integer(), integer()) -> integer().
clamp(Value, Min, _Max) when Value < Min ->
    Min;
clamp(Value, _Min, Max) when Value > Max ->
    Max;
clamp(Value, _Min, _Max) ->
    Value.

-spec audit_admin_action(integer(), binary(), integer(), map()) -> ok.
audit_admin_action(AdmUid, Action, TargetId, Extra) ->
    Body = #{
        <<"action">> => Action,
        <<"operator_uid">> => AdmUid,
        <<"target_id">> => TargetId,
        <<"extra">> => Extra,
        <<"occurred_at">> => elib_dt:now()
    },
    try
        _ = user_log_repo:add(#{
            type => ?ADM_MOMENT_AUDIT_TYPE,
            uid => AdmUid,
            body => jsone:encode(Body, [native_utf8]),
            remark => <<"adm_moment_governance">>,
            created_at => elib_dt:now()
        }),
        ok
    catch
        _:_ ->
            ok
    end.
