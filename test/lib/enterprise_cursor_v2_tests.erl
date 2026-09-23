%% enterprise_cursor_v2_tests
%% V2.1 §10.1 CURSOR-V2 冻结合同的单元测试（§21 CUR-01 的 A1 侧 oracle）。
%%
%% 覆盖：
%%   ① canonical_json：对象 key 按 UTF-8 字节序递归排序；array 保序；
%%      integer/boolean/null 固定 token；禁浮点/非法 atom/非 binary key/
%%      无效 UTF-8；key 顺序不影响编码（幂等）
%%   ② sign/verify roundtrip：base64url 无 padding 双段形态
%%   ③ 负例五类：tampered / malformed / foreign family / 结构非法 / expired
%%      （expired 单独 expired；其余一律 invalid，不回显原因）
%%   ④ signing_key：config 缺失 / 短于 32 bytes / 合法
%%   ⑤ CUR-01 翻页 oracle：keyset 分页不重复不丢行；跨 family/O/App/W 的
%%      payload 绑定比对由 handler 逐字段执行（此处钉住模块层的比较原语）
-module(enterprise_cursor_v2_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, <<"cur_v2_test_signing_secret_0123456789abcdef">>).
-define(KEY_CFG, enterprise_internal_cursor_signing_key).

%%%===================================================================
%%% ① canonical_json
%%%===================================================================

canonical_json_test() ->
    %% 顶层 + 嵌套 key 均按字节序排序；array 保序
    {ok, Bin1} = enterprise_cursor_v2:canonical_json(#{
        <<"z">> => 1, <<"a">> => #{<<"y">> => true, <<"b">> => <<"s">>}
    }),
    ?assertEqual(<<"{\"a\":{\"b\":\"s\",\"y\":true},\"z\":1}">>, Bin1),
    %% key 顺序不影响输出（幂等）
    {ok, Bin2} = enterprise_cursor_v2:canonical_json(#{
        <<"a">> => #{<<"b">> => <<"s">>, <<"y">> => true}, <<"z">> => 1
    }),
    ?assertEqual(Bin1, Bin2),
    %% array 保持输入顺序（不排序）
    {ok, Bin3} = enterprise_cursor_v2:canonical_json(#{<<"l">> => [3, 1, 2]}),
    ?assertEqual(<<"{\"l\":[3,1,2]}">>, Bin3),
    %% boolean/null 固定小写 token；integer 无前导零
    {ok, Bin4} = enterprise_cursor_v2:canonical_json(#{
        <<"t">> => true, <<"f">> => false, <<"n">> => null, <<"i">> => 42, <<"z0">> => 0
    }),
    ?assertEqual(<<"{\"f\":false,\"i\":42,\"n\":null,\"t\":true,\"z0\":0}">>, Bin4),
    %% 空对象/空数组
    {ok, <<"{}">>} = enterprise_cursor_v2:canonical_json(#{}),
    {ok, <<"[]">>} = enterprise_cursor_v2:canonical_json([]),
    %% 禁浮点（含会编码成 NaN/Infinity 的路径）
    ?assertEqual(
        {error, non_canonical}, enterprise_cursor_v2:canonical_json(#{<<"a">> => 1.5})
    ),
    ?assertEqual(
        {error, non_canonical},
        enterprise_cursor_v2:canonical_json(#{<<"a">> => [1, 2.0]})
    ),
    %% 非法 atom / 非 binary key 拒绝
    ?assertEqual(
        {error, non_canonical}, enterprise_cursor_v2:canonical_json(#{<<"a">> => foo})
    ),
    ?assertEqual(
        {error, non_canonical}, enterprise_cursor_v2:canonical_json(#{key => 1})
    ),
    %% 无效 UTF-8 字符串拒绝
    ?assertEqual(
        {error, non_canonical}, enterprise_cursor_v2:canonical_json(#{<<"a">> => <<16#FF>>})
    ).

%%%===================================================================
%%% ② sign/verify roundtrip
%%%===================================================================

roundtrip_test() ->
    Now = os:system_time(second),
    P = enterprise_cursor_v2:build_payload(
        <<"workspaces">>, 101, 202, #{<<"status">> => <<"active">>}, [1700, 42], Now
    ),
    {ok, Cursor} = enterprise_cursor_v2:sign(P, ?SECRET),
    %% 双段形态：base64url 无 padding、恰好一个点
    [EncP, EncM] = binary:split(Cursor, <<".">>, [global]),
    ?assertEqual(2, length(binary:split(Cursor, <<".">>, [global]))),
    ?assertEqual(nomatch, binary:match(EncP, <<"=">>)),
    ?assertEqual(nomatch, binary:match(EncM, <<"=">>)),
    ?assertEqual(43, byte_size(EncM), "SHA-256 base64url 恰 43 字符"),
    %% 验签还原 payload（canonical 编码后 key 有序）
    {ok, Verified} = enterprise_cursor_v2:verify(Cursor, ?SECRET),
    ?assertEqual(2, maps:get(<<"v">>, Verified)),
    ?assertEqual(<<"workspaces">>, maps:get(<<"family">>, Verified)),
    ?assertEqual(101, maps:get(<<"organization_id">>, Verified)),
    ?assertEqual(202, maps:get(<<"application_id">>, Verified)),
    ?assertEqual([1700, 42], maps:get(<<"sort_tuple">>, Verified)),
    ?assertEqual(Now, maps:get(<<"issued_at">>, Verified)),
    %% 8 个 internal family 全部可签可验
    lists:foreach(
        fun(F) ->
            {ok, C} = enterprise_cursor_v2:sign(base_payload(F, Now), ?SECRET),
            ?assertMatch({ok, _}, enterprise_cursor_v2:verify(C, ?SECRET))
        end,
        enterprise_cursor_v2:families()
    ),
    ?assertEqual(8, length(enterprise_cursor_v2:families())).

%%%===================================================================
%%% ③ 负例五类（tampered / malformed / foreign family / 结构非法 / expired）
%%%===================================================================

negatives_test() ->
    Now = os:system_time(second),
    {ok, Cursor} = enterprise_cursor_v2:sign(base_payload(<<"groups">>, Now), ?SECRET),

    %% 1) tampered：改 payload / 换签名段。
    %%    篡改点选**首字符**——base64url 尾字符可能含无显著性的低位填充比特
    %%    （长度非 3 倍数时），翻转尾字符可能解码回同一字节序列；首字符的
    %%    6 bit 恒为有效载荷位，翻转必然改变解码结果。
    [EncP, EncM] = binary:split(Cursor, <<".">>, [global]),
    TamperedP = flip_first_char(EncP),
    {ok, C1} = concat_cursor(TamperedP, EncM),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(C1, ?SECRET)),
    {ok, C2} = concat_cursor(EncP, flip_first_char(EncM)),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(C2, ?SECRET)),
    %% 换密钥验签同样 invalid（不泄露差异）
    ?assertEqual(
        {error, invalid},
        enterprise_cursor_v2:verify(Cursor, <<"wrong-secret-wrong-secret-wrong-secret">>)
    ),

    %% 2) malformed：非双段 / 空 base64url 段 / 非 base64url 字符 / 非 binary
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(<<"not-a-cursor">>, ?SECRET)),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(<<"a.b.c">>, ?SECRET)),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(<<".">>, ?SECRET)),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(<<EncP/binary, ".">>, ?SECRET)),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(<<"!!!.???">>, ?SECRET)),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(123, ?SECRET)),
    %% 签名段非 32 字节（合法 base64url 但长度错）
    ?assertEqual(
        {error, invalid}, enterprise_cursor_v2:verify(<<EncP/binary, ".AAAA">>, ?SECRET)
    ),

    %% 3) foreign family：白名单外的 family（有效签名也不可用）
    Foreign0 = base_payload(<<"groups">>, Now),
    Foreign = Foreign0#{<<"family">> => <<"admin_users">>},
    {ok, C3} = enterprise_cursor_v2:sign(Foreign, ?SECRET),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(C3, ?SECRET)),

    %% 4) 结构非法：v 缺失 / v≠2 / issued_at 非整数 / payload 非对象
    BadV = maps:remove(<<"v">>, base_payload(<<"groups">>, Now)),
    {ok, C4} = enterprise_cursor_v2:sign(BadV, ?SECRET),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(C4, ?SECRET)),
    BadV2 = (base_payload(<<"groups">>, Now))#{<<"v">> => 3},
    {ok, C5} = enterprise_cursor_v2:sign(BadV2, ?SECRET),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(C5, ?SECRET)),
    BadTs = (base_payload(<<"groups">>, Now))#{<<"issued_at">> => <<"now">>},
    {ok, C6} = enterprise_cursor_v2:sign(BadTs, ?SECRET),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(C6, ?SECRET)),
    %% 非对象 payload（数组）——sign 需要 map，直接手造 base64url 数组 + 签名
    {ok, ArrCanon} = enterprise_cursor_v2:canonical_json([1, 2]),
    ArrMac = crypto:mac(hmac, sha256, ?SECRET, ArrCanon),
    C7 = b64c(ArrCanon, ArrMac),
    ?assertEqual({error, invalid}, enterprise_cursor_v2:verify(C7, ?SECRET)),

    %% 5) expired：issued_at + 24h 之外 → 单独 expired（不与 invalid 混同）
    Expired = base_payload(<<"groups">>, Now - enterprise_cursor_v2:ttl_seconds() - 1),
    {ok, C8} = enterprise_cursor_v2:sign(Expired, ?SECRET),
    ?assertEqual({error, expired}, enterprise_cursor_v2:verify(C8, ?SECRET)),
    %% 边界内 1 秒仍有效
    Fresh = base_payload(<<"groups">>, Now - enterprise_cursor_v2:ttl_seconds() + 1),
    {ok, C9} = enterprise_cursor_v2:sign(Fresh, ?SECRET),
    ?assertMatch({ok, _}, enterprise_cursor_v2:verify(C9, ?SECRET)),
    ?assertEqual(86400, enterprise_cursor_v2:ttl_seconds()),

    %% human_directory（A4）在白名单内：同算法可签可验（payload 域不同）
    Human = #{
        <<"v">> => 2,
        <<"family">> => <<"human_directory">>,
        <<"organization_id">> => 101,
        <<"uid">> => 9001,
        <<"filter">> => #{},
        <<"sort_tuple">> => [5],
        <<"issued_at">> => Now
    },
    {ok, C10} = enterprise_cursor_v2:sign(Human, ?SECRET),
    ?assertMatch({ok, _}, enterprise_cursor_v2:verify(C10, ?SECRET)).

%%%===================================================================
%%% ④ signing_key（config 读取；缺失/过短由调用方 503）
%%%===================================================================

signing_key_test() ->
    application:unset_env(imboy, ?KEY_CFG),
    ?assertEqual({error, key_unavailable}, enterprise_cursor_v2:signing_key()),
    application:set_env(imboy, ?KEY_CFG, <<"short">>),
    ?assertEqual({error, key_unavailable}, enterprise_cursor_v2:signing_key()),
    application:set_env(imboy, ?KEY_CFG, ?SECRET),
    ?assertEqual({ok, ?SECRET}, enterprise_cursor_v2:signing_key()),
    %% 非 binary 形态拒绝
    application:set_env(imboy, ?KEY_CFG, 12345),
    ?assertEqual({error, key_unavailable}, enterprise_cursor_v2:signing_key()),
    application:unset_env(imboy, ?KEY_CFG).

%%%===================================================================
%%% ⑤ CUR-01 翻页 oracle：keyset 不重复不丢行 + 绑定比对原语
%%%===================================================================

%% 7 行数据（created_at 降序、id 降序 tie-break——§10.2 groups family 的排序），
%% limit=3 翻 3 页：全行恰好出现一次；游标携带上一页末行的 sort_tuple。
pagination_oracle_test() ->
    Now = os:system_time(second),
    Rows = [
        {1705, 71}, {1704, 70}, {1703, 69}, {1702, 68}, {1701, 67}, {1700, 66}, {1699, 65}
    ],
    {Pages, _LastCursor} = paginate(?SECRET, <<"groups">>, Rows, 3, Now, [], undefined),
    Flattened = lists:append(lists:reverse(Pages)),
    %% 恰好 7 行（3+3+1），无重复、无丢失，顺序服从 keyset
    ?assertEqual(7, length(Flattened)),
    ?assertEqual(7, length(lists:usort(Flattened))),
    ?assertEqual(Rows, Flattened),
    ?assertEqual(3, length(Pages)).

paginate(_Secret, _Family, [], _Limit, _Now, Pages, Cursor) ->
    {Pages, Cursor};
paginate(Secret, Family, Rows, Limit, Now, Pages, Cursor) ->
    %% 用游标过滤（keyset：created_at DESC, id DESC → 保留 < sort_tuple 的行）
    Remaining =
        case Cursor of
            undefined ->
                Rows;
            _ ->
                {ok, P} = enterprise_cursor_v2:verify(Cursor, Secret),
                Pivot = sort_tuple_of(P),
                [R || R <- Rows, R < Pivot]
        end,
    Page = lists:sublist(Remaining, Limit),
    case Page of
        [] ->
            {Pages, Cursor};
        _ ->
            {LastCa, LastId} = lists:last(Page),
            Next = enterprise_cursor_v2:build_payload(
                Family, 101, 202, #{}, [LastCa, LastId], Now
            ),
            {ok, NextCursor} = enterprise_cursor_v2:sign(Next, Secret),
            paginate(Secret, Family, Rows, Limit, Now, [Page | Pages], NextCursor)
    end.

sort_tuple_of(P) ->
    [Ca, Id] = maps:get(<<"sort_tuple">>, P),
    {Ca, Id}.

%% handler 侧绑定比对原语（§10.1 foreign O/App/W/filter → 400 invalid_request，
%% 不得自动改写）：A2/A4 在 verify 成功后逐字段比较当前请求上下文。
binding_oracle_test() ->
    Now = os:system_time(second),
    %% 当前请求：family=workspaces, O=101, App=202, W=301
    BaseP = base_payload(<<"workspaces">>, Now),
    BoundP = BaseP#{<<"filter">> => #{<<"workspace_id">> => 301}},
    {ok, Cursor} = enterprise_cursor_v2:sign(BoundP, ?SECRET),
    {ok, P} = enterprise_cursor_v2:verify(Cursor, ?SECRET),
    ?assertEqual(true, binds_current(P, <<"workspaces">>, 101, 202, 301)),
    %% foreign family / O / App / W：逐字段比较失败（handler 须 400，不得改写）
    ?assertEqual(false, binds_current(P, <<"groups">>, 101, 202, 301)),
    ?assertEqual(false, binds_current(P, <<"workspaces">>, 999, 202, 301)),
    ?assertEqual(false, binds_current(P, <<"workspaces">>, 101, 999, 301)),
    ?assertEqual(false, binds_current(P, <<"workspaces">>, 101, 202, 999)).

binds_current(P, Family, OrgId, AppId, WsId) ->
    maps:get(<<"family">>, P, undefined) =:= Family andalso
        maps:get(<<"organization_id">>, P, undefined) =:= OrgId andalso
        maps:get(<<"application_id">>, P, undefined) =:= AppId andalso
        maps:get(<<"workspace_id">>, maps:get(<<"filter">>, P, #{}), undefined) =:= WsId.

%%%===================================================================
%%% Internal
%%%===================================================================

base_payload(Family, Now) ->
    enterprise_cursor_v2:build_payload(Family, 101, 202, #{}, [1, 2], Now).

flip_first_char(Bin) ->
    Size = byte_size(Bin),
    Head = binary:part(Bin, 1, Size - 1),
    First = binary:first(Bin),
    <<(flip(First)):8, Head/binary>>.

flip(C) when C >= $a, C =< $y -> C + 1;
flip($z) -> $a;
flip(C) when C >= $A, C =< $Y -> C + 1;
flip($Z) -> $A;
flip(C) when C >= $0, C =< $8 -> C + 1;
flip($9) -> $0;
flip(C) -> C bxor 1.

concat_cursor(EncP, EncM) ->
    {ok, <<EncP/binary, ".", EncM/binary>>}.

b64c(Payload, Mac) ->
    <<(b64u(Payload))/binary, ".", (b64u(Mac))/binary>>.

b64u(Data) ->
    base64:encode(Data, #{padding => false, mode => url}).
