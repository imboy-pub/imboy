-module(log_redact).

%%%===================================================================
%%% @doc 日志脱敏共享层（Overseas Compliance Plan Task V-02）
%%%
%%% 接入点是唯一的 sink：elib_log:safe_log 在交给 lager 之前对日志
%%% 做「键 + 值」两轮脱敏，全仓调用点无需改动。
%%%
%%% 两轮语义：
%%%   term/1 —— 键脱敏（精确键名匹配，map/proplist 递归）+ binary 值
%%%             的模式脱敏；在字符串化之前跑，键值关系还在。
%%%   text/1 —— 字符串上的值模式兜底（JWT/Bearer/手机号/邮箱/URL 敏感
%%%             参数）；兜住 term 轮抓不住的字符串内嵌秘密。
%%%
%%% 判定口径与 user_export_logic:sensitive_key/1 对齐（宁可多剥不可
%%% 漏剥）；键采用精确匹配而非子串，避免 design/assignment 这类误伤。
%%% 裸 code 键不脱敏（error_code/http code 误伤面太广），验证码用
%%% verify_code/sms_code/verification_code 精确键覆盖，调用点纪律为
%%% 「验证码不进日志」的最后防线仍是 code review。
%%%
%%% fail-closed：脱敏自身出错时返回 [REDACT_ERROR] 占位，绝不回退
%%% 原文——日志丢失可排查，秘密泄漏不可撤回。
%%% @end
%%%===================================================================

-export([term/1, text/1]).

-ifdef(TEST).
-export([forbidden_key/1]).
-endif.

%% 预编译正则缓存在 persistent_term（进程共享，编译一次）
-define(JWT_MP, log_redact_jwt_mp).
-define(BEARER_MP, log_redact_bearer_mp).
-define(MOBILE_MP, log_redact_mobile_mp).
-define(EMAIL_MP, log_redact_email_mp).
-define(URL_SECRET_MP, log_redact_url_secret_mp).

-define(REDACTED, <<"[REDACTED]">>).

%% @doc term 形态脱敏：map/proplist 键精确匹配 + binary 值模式。
-spec term(term()) -> term().
term(M) when is_map(M) ->
    maps:fold(
        fun(K, V, Acc) ->
            case forbidden_key(K) of
                true -> Acc#{K => ?REDACTED};
                false -> Acc#{K => term(V)}
            end
        end,
        #{},
        M
    );
term([{K, _V} | Rest] = PropList) when (is_atom(K) orelse is_binary(K) orelse is_list(K)) ->
    case lists:all(fun(E) -> is_tuple(E) andalso tuple_size(E) =:= 2 end, Rest) of
        true ->
            %% proplist：键值对逐项判定
            [
                case is_key_like(K0) andalso forbidden_key(K0) of
                    true -> {K0, ?REDACTED};
                    false -> {K0, term(V0)}
                end
             || {K0, V0} <- PropList
            ];
        false ->
            [term(E) || E <- PropList]
    end;
term(L) when is_list(L) ->
    [term(E) || E <- L];
term(B) when is_binary(B) ->
    text(B);
term(T) ->
    T.

%% @doc 字符串值模式兜底：JWT / Bearer / 手机号 / 邮箱 / URL 敏感参数。
%% char list 输入（ensure_string 产物）转 binary 处理后返回 binary。
-spec text(term()) -> term().
text(L) when is_list(L) ->
    text(unicode:characters_to_binary(L));
text(B) when is_binary(B) ->
    S = unicode:characters_to_binary(B),
    S1 = re_replace(
        S,
        ?JWT_MP,
        <<"eyJ[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+">>,
        <<"[REDACTED]">>
    ),
    S2 = re_replace(S1, ?BEARER_MP, "(?i)bearer[[:space:]]+[A-Za-z0-9._~-]+", <<"[REDACTED]">>),
    %% 手机号：捕获组保持前后分隔符，避免吞掉相邻字符
    S3 = re_replace(
        S2,
        ?MOBILE_MP,
        "(^|[^0-9])1[3-9][0-9]{9}([^0-9]|$)",
        "\\1[REDACTED]\\2"
    ),
    S4 = re_replace(
        S3,
        ?EMAIL_MP,
        "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}",
        <<"[REDACTED]">>
    ),
    %% URL 敏感参数：保留参数名，只清参数值（\1 是参数名捕获组）
    re_replace(
        S4,
        ?URL_SECRET_MP,
        "(?i)([?&](?:token|access_token|refresh_token|sign|signature|ticket|auth)=)[^&[:space:]]+",
        "\\1[REDACTED]"
    );
text(T) ->
    T.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 键名是否命中禁止集（大小写不敏感；atom/binary/字符串统一判定）。
%% 精确匹配集合：与 user_export_logic 的子串口径不同——日志键来自代码
%% 与协议字段，精确集足以覆盖且不会把 design/assignment 误判为 sign。
-spec forbidden_key(term()) -> boolean().
forbidden_key(K) when is_atom(K); is_binary(K); is_list(K) ->
    Lower = lower(K),
    lists:member(
        Lower,
        [
            <<"password">>,
            <<"passwd">>,
            <<"pwd">>,
            <<"secret">>,
            <<"secret_key">>,
            <<"client_secret">>,
            <<"token">>,
            <<"access_token">>,
            <<"refresh_token">>,
            <<"id_token">>,
            <<"private_key">>,
            <<"privatekey">>,
            <<"salt">>,
            <<"password_salt">>,
            <<"credential">>,
            <<"credentials">>,
            <<"api_key">>,
            <<"apikey">>,
            <<"access_key">>,
            <<"authorization">>,
            <<"auth">>,
            <<"cookie">>,
            <<"set-cookie">>,
            <<"sign">>,
            <<"signature">>,
            <<"openid">>,
            <<"session_key">>,
            <<"verify_code">>,
            <<"sms_code">>,
            <<"verification_code">>,
            <<"email_code">>,
            <<"plaintext">>,
            <<"plain_text">>,
            <<"plain">>,
            <<"msg_content">>,
            <<"message_content">>,
            <<"password_hash">>,
            <<"passhash">>
        ]
    );
forbidden_key(_) ->
    false.

is_key_like(K) when is_atom(K); is_binary(K); is_list(K) -> true;
is_key_like(_) -> false.

-spec lower(term()) -> binary().
lower(K) when is_atom(K) ->
    lower(atom_to_binary(K, utf8));
lower(K) when is_binary(K) ->
    string:lowercase(K);
lower(K) when is_list(K) ->
    lower(unicode:characters_to_binary(K));
lower(_) ->
    <<>>.

%% 正则按需编译进 persistent_term（首次调用编译，此后进程共享）
-spec mp(term(), binary()) -> term().
mp(CacheKey, Pattern) ->
    case persistent_term:get(CacheKey, undefined) of
        undefined ->
            {ok, Compiled} = re:compile(Pattern),
            ok = persistent_term:put(CacheKey, Compiled),
            Compiled;
        Compiled ->
            Compiled
    end.

-spec re_replace(binary(), term(), iodata(), iodata()) -> binary().
re_replace(Subject0, CacheKey, Pattern, Replacement) ->
    try
        MP = mp(CacheKey, iolist_to_binary(Pattern)),
        %% re:replace 直接返回替换后的串（{ok, ...} 包装是 re:run 的形态）
        re:replace(Subject0, MP, Replacement, [global, {return, binary}])
    catch
        _:_ ->
            %% 正则环节失败宁可整串脱敏，不回退原文
            <<"[REDACT_ERROR]">>
    end.
