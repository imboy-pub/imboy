-module(talk_record_content_common).
-moduledoc "面聊内容域共享逻辑（T15）：ETag/If-Match 惯例、角色门禁、q 高亮。".
%%%
% segments / speakers / summary 三域共用的纯逻辑与门禁（T15）。
%
% ETag / If-Match 惯例（与 T16 UI 冻结契约对齐）：
%   * segment：`segment:<id>:<revision>`；speaker：`speaker:<id>:<revision>`；
%     summary：`summary:<revision>`（T16 fixture 纯函数
%     talkRecordSegmentEtagOf/talkRecordSpeakerEtagOf 同式派生）；
%   * HTTP 响应头为 RFC 引号形态 `"segment:1:2"`；payload etag 字段为不带
%     引号的令牌本体（UI 直接比对/回传）；
%   * If-Match 接受三种等价形态：`segment:<id>:<rev>`（令牌）、`"N"`/N
%     （T05/T07 域内既有数字形态，向后兼容）；令牌携带的 id 与路径子 ID
%     不一致 → 422 invalid_if_match（防拿 A 行令牌改 B 行）。
%
% 版本域纪律（§11.2）：segments/speakers 编辑推 record.content_revision；
% summary 编辑/确认推自身 revision；三者均不碰 record.revision /
% acl_revision（notes 编辑仍走 T05 PATCH /:id 的 revision 域）。
%%%

-export([
    parse_resource_if_match/3,
    segment_etag/2,
    speaker_etag/2,
    summary_etag/1,
    visible_or_404/2,
    editable_role_gate/2,
    actor_role/2,
    highlight_positions/2,
    valid_text/2,
    validate_fields/3,
    parse_pos_int/1
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% ETag / If-Match
%% ===================================================================

-spec segment_etag(integer(), pos_integer()) -> binary().
segment_etag(Id, Revision) ->
    resource_etag(<<"segment">>, Id, Revision).

-spec speaker_etag(integer(), pos_integer()) -> binary().
speaker_etag(Id, Revision) ->
    resource_etag(<<"speaker">>, Id, Revision).

-spec summary_etag(pos_integer()) -> binary().
summary_etag(Revision) ->
    <<"summary:", (integer_to_binary(Revision))/binary>>.

-spec resource_etag(binary(), integer(), pos_integer()) -> binary().
resource_etag(Prefix, Id, Revision) ->
    <<Prefix/binary, ":", (integer_to_binary(Id))/binary, ":",
        (integer_to_binary(Revision))/binary>>.

%% @doc If-Match 解析（缺头 428；形态非法 422）。
%% 接受：`segment:<id>:<rev>` / `speaker:<id>:<rev>`（三段令牌）、
%% `summary:<rev>`（两段令牌）——带/不带引号、W/ 前缀——或纯 revision
%% （`"N"` / N，T05/T07 兼容）。三段令牌 id 段须与路径 Id 一致。
-spec parse_resource_if_match(binary(), integer(), binary() | undefined) ->
    {ok, pos_integer()} | {error, atom()}.
parse_resource_if_match(_Prefix, _Id, undefined) ->
    {error, precondition_required};
parse_resource_if_match(Prefix, Id, Bin) when is_binary(Bin) ->
    Token = strip_etag(Bin),
    case binary:split(Token, <<":">>, [global]) of
        [Prefix, IdBin, RevBin] ->
            case IdBin =:= integer_to_binary(Id) of
                true -> parse_revision(RevBin);
                false -> {error, invalid_if_match}
            end;
        [Prefix, RevBin] ->
            parse_revision(RevBin);
        [RevBin] ->
            parse_revision(RevBin);
        _ ->
            {error, invalid_if_match}
    end.

%% @private 去引号与 W/ 弱标记（与 T05 parse_if_match 同语义）。
-spec strip_etag(binary()) -> binary().
strip_etag(Bin) ->
    NoWeak = binary:replace(Bin, <<"W/">>, <<>>, [global]),
    binary:replace(NoWeak, <<"\"">>, <<>>, [global]).

-spec parse_revision(binary()) -> {ok, pos_integer()} | {error, atom()}.
parse_revision(RevBin) ->
    try
        case binary_to_integer(RevBin) of
            Rev when Rev >= 1 -> {ok, Rev};
            _ -> {error, invalid_if_match}
        end
    catch
        _:_ -> {error, invalid_if_match}
    end.

%% ===================================================================
%% ACL / 角色
%% ===================================================================

%% @doc 可见性统一入口（§11.3：无权限/不存在统一 404）。
-spec visible_or_404(integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
visible_or_404(Uid, RecordId) ->
    talk_record_repo:find_visible(Uid, RecordId).

%% @doc 可见行上的角色（owner / grant editor / grant viewer；与
%% talk_record_ds:actor_role 同口径，内容域独立复用）。
-spec actor_role(integer(), map()) -> owner | editor | viewer.
actor_role(Uid, #{<<"owner_user_id">> := Uid}) ->
    owner;
actor_role(Uid, #{<<"id">> := RecordId}) ->
    case talk_record_repo:find_active_grant(RecordId, Uid) of
        {ok, #{<<"role">> := <<"editor">>}} -> editor;
        _ -> viewer
    end.

%% @doc 编辑门禁：owner/editor 放行；viewer（有读权限但无编辑能力）403
%% （§1/§4：PATCH segments/speakers/summary 为校对权）。
-spec editable_role_gate(integer(), map()) -> ok | {error, atom()}.
editable_role_gate(Uid, Row) ->
    case actor_role(Uid, Row) of
        Role when Role =:= owner; Role =:= editor -> ok;
        viewer -> {error, forbidden}
    end.

%% ===================================================================
%% q 高亮（§4：q 命中返回高亮位置，不回全文扫描日志）
%% ===================================================================

%% @doc 大小写不敏感非重叠匹配位置（Unicode codepoint 偏移，[start, end)）。
%% 与 UI 端 Dart String 偏移同单位（UTF-16 ≈ codepoint，增补面字符除外，
%% 已在 T15 报告登记单位约定）。q 为空 → []。
-spec highlight_positions(binary(), binary()) -> [map()].
highlight_positions(Text, <<>>) when is_binary(Text) ->
    [];
highlight_positions(Text, Q) when is_binary(Text), is_binary(Q) ->
    LText = string:lowercase(Text),
    LQ = string:lowercase(Q),
    case byte_size(LQ) of
        0 ->
            [];
        _ ->
            QCpLen = string:length(LQ),
            scan_matches(LText, LQ, 0, 0, QCpLen, [])
    end.

%% @private 从字节偏移 ScanStart 起找下一处匹配；CpBase 为 ScanStart 处的
%% codepoint 累计偏移。匹配间跳跃段的 codepoint 数用 string:length 计。
-spec scan_matches(
    binary(),
    binary(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    [map()]
) -> [map()].
scan_matches(LText, LQ, ScanStart, CpBase, QCpLen, Acc) ->
    Scope = {ScanStart, byte_size(LText) - ScanStart},
    case binary:match(LText, LQ, [{scope, Scope}]) of
        nomatch ->
            lists:reverse(Acc);
        {MatchStart, MatchLen} ->
            Skipped = binary:part(LText, ScanStart, MatchStart - ScanStart),
            StartCp = CpBase + string:length(Skipped),
            scan_matches(
                LText,
                LQ,
                MatchStart + MatchLen,
                StartCp + QCpLen,
                QCpLen,
                [#{<<"start">> => StartCp, <<"end">> => StartCp + QCpLen} | Acc]
            )
    end.

%% ===================================================================
%% 输入校验
%% ===================================================================

%% @doc 文本长度校验（codepoint 上限 MaxChars）。
-spec valid_text(term(), pos_integer()) -> boolean().
valid_text(Text, MaxChars) when is_binary(Text) ->
    string:length(Text) =< MaxChars;
valid_text(_, _) ->
    false.

%% @doc 字段校验器列表驱动（segments/speakers/summary 共用）：
%% [{Key, Check(value)->bool, Reason}]，只校验 Picked 中存在的键；
%% 全部通过返回 {ok, Picked}。
-spec validate_fields([{binary(), function(), atom()}], map(), ok | {error, atom()}) ->
    {ok, map()} | {error, atom()}.
validate_fields(Checks, Picked, ok) ->
    do_validate_fields(Checks, Picked);
validate_fields(_Checks, _Picked, {error, Reason}) ->
    {error, Reason}.

-spec do_validate_fields([{binary(), function(), atom()}], map()) ->
    {ok, map()} | {error, atom()}.
do_validate_fields([], Picked) ->
    {ok, Picked};
do_validate_fields([{Key, Check, Reason} | Rest], Picked) ->
    case maps:find(Key, Picked) of
        {ok, Value} ->
            case Check(Value) of
                true -> do_validate_fields(Rest, Picked);
                false -> {error, Reason}
            end;
        error ->
            do_validate_fields(Rest, Picked)
    end.

%% @doc 正整数解析（JSON int64 / 数字 binary）。
-spec parse_pos_int(term()) -> {ok, pos_integer()} | error.
parse_pos_int(N) when is_integer(N), N > 0 -> {ok, N};
parse_pos_int(B) when is_binary(B) ->
    try
        parse_pos_int(binary_to_integer(B))
    catch
        _:_ -> error
    end;
parse_pos_int(_) ->
    error.
