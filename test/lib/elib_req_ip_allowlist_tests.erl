-module(elib_req_ip_allowlist_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc elib_req:ip_in_allowlist/2 语义锁定（A-04）
%%%
%%% 该函数是两道 IP 门的共同判定：管理后台 adm_ip_allowlist 与
%%% 支付回调 payment_callback_ip_allowlist。两者都是"命中才放行"，
%%% 所以本函数任何一次误判 true 都直接等于放行。
%%%
%%% 锁定的不变量：
%%%   1. 空白名单 → false（"未配置 = 不启用"由调用方决定，不在此兜底）
%%%   2. 空串条目 → 不命中（空前缀会匹配一切，等于关掉整道门）
%%%   3. 前缀匹配按字节边界，不做"看起来像同段"的宽松推断
%%% @end
%%%===================================================================

%% 空列表不能命中。调用方（adm_auth_middleware / payment_callback_handler）
%% 各自判空决定是否启用；此处若返回 true，漏判空的调用方就静默全放行。
empty_allowlist_never_matches_test() ->
    ?assertEqual(false, elib_req:ip_in_allowlist(<<"127.0.0.1">>, [])).

exact_match_test() ->
    ?assertEqual(true, elib_req:ip_in_allowlist(<<"110.75.1.2">>, [<<"110.75.1.2">>])),
    ?assertEqual(true, elib_req:ip_in_allowlist(<<"110.75.1.2">>, ["110.75.1.2"])).

no_match_test() ->
    ?assertEqual(false, elib_req:ip_in_allowlist(<<"203.0.113.9">>, [<<"110.75.1.2">>])).

%% 前缀条目（网关按段公布出口 IP 时的常见写法）
prefix_match_test() ->
    ?assertEqual(true, elib_req:ip_in_allowlist(<<"110.75.1.2">>, [<<"110.75.">>])),
    ?assertEqual(true, elib_req:ip_in_allowlist(<<"110.75.1.2">>, [<<"1.1.1.1">>, "110.75."])).

%% 回归：空串条目曾会命中任意 IP（binary:longest_common_prefix 对空前缀恒真），
%% 配置里多打一个 "" 就等于整道门形同虚设。
empty_entry_does_not_match_everything_test() ->
    ?assertEqual(false, elib_req:ip_in_allowlist(<<"203.0.113.9">>, [<<>>])),
    ?assertEqual(false, elib_req:ip_in_allowlist(<<"203.0.113.9">>, [""])),
    ?assertEqual(false, elib_req:ip_in_allowlist(<<"203.0.113.9">>, [<<>>, <<"110.75.">>])).

%% get_client_ip/1 理论上恒返回 binary，但白名单是安全判定，
%% 非 binary 入参必须落在"不命中"这一侧而不是崩掉或放行。
non_binary_ip_never_matches_test() ->
    ?assertEqual(false, elib_req:ip_in_allowlist(undefined, [<<"110.75.">>])),
    ?assertEqual(false, elib_req:ip_in_allowlist("110.75.1.2", [<<"110.75.">>])).

%% 配置项被误写成非列表（例如单个 binary）时不得崩掉请求
non_list_allowlist_never_matches_test() ->
    ?assertEqual(false, elib_req:ip_in_allowlist(<<"110.75.1.2">>, <<"110.75.">>)).

%% 非字符串条目（配置写成 atom/整数）归一为不命中，不崩
garbage_entry_is_ignored_test() ->
    ?assertEqual(false, elib_req:ip_in_allowlist(<<"110.75.1.2">>, [undefined, 110, {a, b}])),
    ?assertEqual(true, elib_req:ip_in_allowlist(<<"110.75.1.2">>, [undefined, <<"110.75.">>])).

%% 前缀比较按字节，不因"同一个 /24 段"就放行
prefix_is_byte_wise_not_subnet_aware_test() ->
    %% "10.0.0." 不匹配 "10.0.1.5"
    ?assertEqual(false, elib_req:ip_in_allowlist(<<"10.0.1.5">>, [<<"10.0.0.">>])),
    %% 但 "10.0.1" 会匹配 "10.0.11.5" —— 这是纯前缀语义的已知边界，
    %% 配置时应带上末尾的点。此断言把该行为钉住，防止有人误以为它懂子网。
    ?assertEqual(true, elib_req:ip_in_allowlist(<<"10.0.11.5">>, [<<"10.0.1">>])).
