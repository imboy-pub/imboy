-module(user_export_logic).

%%%===================================================================
%%% @doc 个人数据导出（GDPR 第 20 条 数据可携带权）
%%%
%%% 受限范围：只导出「当前登录用户自己」的数据。Uid 必须来自 auth 中间件
%%% 注入的 current_uid，绝不接受请求参数——否则任意用户可导出他人数据。
%%%
%%% 数据来源为 user_ds:export_data_bounded/1（有界导出：显式列 allowlist +
%%% friends/groups LIMIT 截断标记）；本模块负责四件事：冷却限频、范围校验、
%%% 敏感字段兜底剥离、导出行为审计。
%%%
%%% 敏感字段剥离的必要性：即使 DS 层已用显式列，user_setting.setting 是
%%% jsonb 自由键（将来新增设置项自动出现），黑名单兜底保证「新增敏感键
%%% 不会静默泄漏」。与列 allowlist 构成双层防线。
%%% @end
%%%===================================================================

-export([export/2]).
-export([sanitize/1, sensitive_key/1, legal_hold_status/0, cool_down_ms/0, scope/0]).

-include("log.hrl").

%% user_log.type：100=登录 120=管理员操作 130=个人数据导出
-define(LOG_TYPE_DATA_EXPORT, 130).

%% 导出冷却窗口默认 24h（毫秒）；配置 user_export_cool_down_ms=0 可关闭。
-define(DEFAULT_COOL_DOWN_MS, 86400000).

%% @doc 导出指定用户自己的数据。Uid 非法直接拒绝，不回退到任何默认账号。
%% 冷却门（P-01）：上次导出审计行后冷却窗口内重复请求拒绝，防止反复导出刷库；
%% 检查本身故障时放行（数据权优先，与审计失败不阻断同向）。
%% 数据源为 user_ds:export_data_bounded/1（有界 + 显式列 allowlist）。
-spec export(integer(), cowboy_req:req()) -> {ok, map()} | {error, term()}.
export(Uid, Req) when is_integer(Uid), Uid > 0 ->
    case check_cool_down(Uid) of
        ok ->
            case user_ds:export_data_bounded(Uid) of
                {ok, Raw} ->
                    Data = sanitize(Raw),
                    ok = audit(Uid, Req),
                    {ok, Data#{
                        <<"legal_hold">> => legal_hold_status(),
                        <<"scope">> => scope()
                    }};
                {error, Reason} ->
                    ?WARN_LOG([user_export_failed, Uid, Reason]),
                    {error, Reason}
            end;
        {error, _CoolDown} = Rej ->
            Rej
    end;
export(_Uid, _Req) ->
    {error, invalid_uid}.

%% @doc 导出冷却窗口（毫秒）。0 = 关闭冷却（测试/运维用）。
-spec cool_down_ms() -> non_neg_integer().
cool_down_ms() ->
    application:get_env(imboy, user_export_cool_down_ms, ?DEFAULT_COOL_DOWN_MS).

%% @doc 冷却检查：user_log type=130 审计行即频控真源。
%% 上次导出时间 + 冷却窗口 > 当前时刻 → {error, {cool_down, 剩余毫秒}}。
%% 查询/解析失败一律放行（fail-open）并留 ERROR 日志，不因冷却门故障拒绝数据权。
-spec check_cool_down(integer()) -> ok | {error, {cool_down, non_neg_integer()}}.
check_cool_down(Uid) ->
    case cool_down_ms() of
        0 ->
            ok;
        Window ->
            case user_log_ds:last_export_at(Uid) of
                {ok, undefined} ->
                    ok;
                {ok, LastAt} ->
                    judge_cool_down(Uid, LastAt, Window);
                {error, Reason} ->
                    ?ERROR_LOG([user_export_cool_down_check_failed, Uid, Reason]),
                    ok
            end
    end.

-spec judge_cool_down(integer(), binary(), non_neg_integer()) ->
    ok | {error, {cool_down, non_neg_integer()}}.
judge_cool_down(Uid, LastAt, Window) ->
    try elib_dt:rfc3339_to(LastAt, millisecond) of
        LastMs when is_integer(LastMs) ->
            Elapsed = elib_dt:millisecond() - LastMs,
            case Elapsed < Window of
                true -> {error, {cool_down, Window - Elapsed}};
                false -> ok
            end;
        _ ->
            ?ERROR_LOG([user_export_cool_down_bad_timestamp, Uid, LastAt]),
            ok
    catch
        _:Reason ->
            ?ERROR_LOG([user_export_cool_down_bad_timestamp, Uid, LastAt, Reason]),
            ok
    end.

%% @doc 导出范围声明（P-01）：范围由 legal review 决定，本响应不声称 GDPR 完整。
%% 消息/动态/附件等大表的异步归档导出未提供（计划 "async job if needed" 项），
%% excluded 里逐项点名，供用户与审计方核对——静默缺失会被误读为已覆盖。
-spec scope() -> map().
scope() ->
    #{
        <<"categories">> =>
            [<<"user_info">>, <<"friends">>, <<"groups">>, <<"settings">>],
        <<"friends_limit">> => application:get_env(imboy, export_friends_limit, 5000),
        <<"groups_limit">> => application:get_env(imboy, export_groups_limit, 1000),
        <<"excluded">> =>
            [<<"messages">>, <<"moments">>, <<"attachments">>, <<"payments">>],
        <<"disclaimer">> =>
            <<
                "本导出仅包含上述分类的即时数据，不声称满足完整的数据可携带权；"
                "消息、动态、附件等未包含，扩展范围由法务评审决定。"/utf8
            >>
    }.

%% @doc Legal Hold（诉讼保全）未实现，显式声明不支持。
%% 静默省略会让合规审计误以为已支持——宁可明确说没有。
-spec legal_hold_status() -> map().
legal_hold_status() ->
    #{
        <<"supported">> => false,
        <<"reason">> =>
            <<"本版本未实现 Legal Hold（诉讼保全），导出内容不包含保全标记。"/utf8>>
    }.

%% @doc 递归剥离敏感字段：map 逐键过滤，list 逐元素递归。
-spec sanitize(term()) -> term().
sanitize(M) when is_map(M) ->
    maps:fold(
        fun(K, V, Acc) ->
            case sensitive_key(K) of
                true -> Acc;
                false -> Acc#{K => sanitize(V)}
            end
        end,
        #{},
        M
    );
sanitize(L) when is_list(L) ->
    [sanitize(E) || E <- L];
sanitize(V) ->
    V.

%% @doc 键名是否敏感（大小写不敏感的子串匹配，宁可多剥不可漏剥）
-spec sensitive_key(term()) -> boolean().
sensitive_key(K) when is_atom(K) ->
    sensitive_key(atom_to_binary(K, utf8));
sensitive_key(K) when is_binary(K) ->
    Lower = string:lowercase(K),
    lists:any(
        fun(Pat) -> binary:match(Lower, Pat) =/= nomatch end,
        [
            <<"password">>,
            <<"passwd">>,
            <<"secret">>,
            <<"token">>,
            <<"private">>,
            <<"salt">>,
            <<"credential">>,
            <<"api_key">>,
            <<"apikey">>,
            <<"access_key">>,
            <<"secret_key">>
        ]
    );
sensitive_key(_) ->
    false.

%% @doc 导出行为审计：不可变追加，写入 user_log。
%% 审计失败不阻断导出（用户的数据权优先），但必须留下 ERROR 日志。
-spec audit(integer(), cowboy_req:req()) -> ok.
audit(Uid, Req) ->
    Body = jsone:encode(#{
        <<"action">> => <<"user_data_export">>,
        <<"ip">> => header(<<"x-forwarded-for">>, Req),
        <<"did">> => header(<<"did">>, Req),
        <<"vsn">> => header(<<"vsn">>, Req)
    }),
    try user_log_ds:add_internal(undefined, ?LOG_TYPE_DATA_EXPORT, Uid, Body, elib_dt:now()) of
        {error, Reason} ->
            ?ERROR_LOG([user_export_audit_failed, Uid, Reason]),
            ok;
        _ ->
            ok
    catch
        Class:Reason2 ->
            ?ERROR_LOG([user_export_audit_failed, Uid, Class, Reason2]),
            ok
    end.

-spec header(binary(), cowboy_req:req()) -> binary().
header(Name, Req) ->
    try cowboy_req:header(Name, Req, <<>>) of
        V when is_binary(V) -> V;
        _ -> <<>>
    catch
        _:_ -> <<>>
    end.
