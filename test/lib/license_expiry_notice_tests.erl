-module(license_expiry_notice_tests).
-include_lib("eunit/include/eunit.hrl").

%%% @doc license_expiry_notice 到期阈值判定 + 文案渲染测试（纯逻辑，无 IO）。

-define(DAY, 86400000).
-define(NOW, 1700000000000).

due_threshold_test_() ->
    Now = ?NOW,
    [
        %% 超过最大档 → 不提醒
        ?_assertEqual(none, license_expiry_notice:due_threshold(Now + 35 * ?DAY, Now)),
        %% 正好 30 天 / 8 天 → 落 30 档
        ?_assertEqual(30, license_expiry_notice:due_threshold(Now + 30 * ?DAY, Now)),
        ?_assertEqual(30, license_expiry_notice:due_threshold(Now + 8 * ?DAY, Now)),
        %% 7 天 / 5 天 → 落 7 档（最紧急命中）
        ?_assertEqual(7, license_expiry_notice:due_threshold(Now + 7 * ?DAY, Now)),
        ?_assertEqual(7, license_expiry_notice:due_threshold(Now + 5 * ?DAY, Now)),
        %% 1 天 → 落 1 档
        ?_assertEqual(1, license_expiry_notice:due_threshold(Now + 1 * ?DAY, Now)),
        %% 正好到期 / 已过期 → expired
        ?_assertEqual(expired, license_expiry_notice:due_threshold(Now, Now)),
        ?_assertEqual(expired, license_expiry_notice:due_threshold(Now - 3 * ?DAY, Now))
    ].

render_test_() ->
    [
        fun() ->
            {Subj, Body} = license_expiry_notice:render(7, <<"测试公司"/utf8>>),
            ?assertNotEqual(nomatch, binary:match(Subj, <<"7">>)),
            ?assertNotEqual(nomatch, binary:match(Body, <<"测试公司"/utf8>>))
        end,
        fun() ->
            {Subj, _Body} = license_expiry_notice:render(expired, <<"X">>),
            ?assertNotEqual(nomatch, binary:match(Subj, <<"过期"/utf8>>))
        end
    ].
