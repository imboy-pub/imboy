-module(user_export_logic).

%%%===================================================================
%%% @doc 个人数据导出（GDPR 第 20 条 数据可携带权）
%%%
%%% 受限范围：只导出「当前登录用户自己」的数据。Uid 必须来自 auth 中间件
%%% 注入的 current_uid，绝不接受请求参数——否则任意用户可导出他人数据。
%%%
%%% 数据来源复用既有的 user_ds:export_data/1（user_deletion_logic 已在用），
%%% 本模块负责三件事：范围校验、敏感字段兜底剥离、导出行为审计。
%%%
%%% 敏感字段剥离的必要性：user_ds:export_data/1 对 user_setting 表用的是
%%% `SELECT *`，将来新增列（如凭据类字段）会自动流进导出结果。白名单式改造
%%% 涉及跨模块，这里先做黑名单兜底，保证「新增敏感列不会静默泄漏」。
%%% @end
%%%===================================================================

-export([export/2]).
-export([sanitize/1, sensitive_key/1, legal_hold_status/0]).

-include("log.hrl").

%% user_log.type：100=登录 120=管理员操作 130=个人数据导出
-define(LOG_TYPE_DATA_EXPORT, 130).

%% @doc 导出指定用户自己的数据。Uid 非法直接拒绝，不回退到任何默认账号。
-spec export(integer(), cowboy_req:req()) -> {ok, map()} | {error, term()}.
export(Uid, Req) when is_integer(Uid), Uid > 0 ->
    case user_ds:export_data(Uid) of
        {ok, Raw} ->
            Data = sanitize(Raw),
            ok = audit(Uid, Req),
            {ok, Data#{<<"legal_hold">> => legal_hold_status()}};
        {error, Reason} ->
            ?WARN_LOG([user_export_failed, Uid, Reason]),
            {error, Reason}
    end;
export(_Uid, _Req) ->
    {error, invalid_uid}.

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
