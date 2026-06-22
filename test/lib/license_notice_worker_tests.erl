-module(license_notice_worker_tests).
-include_lib("eunit/include/eunit.hrl").

%%% @doc license_notice_worker:should_send/2 防重决策测试（纯函数）。
%%% 不触发发信——发信路径需启用+收件人配置，属集成层不在单测范围。

should_send_test_() ->
    [
        %% 无命中档：永不发
        ?_assertEqual(false, license_notice_worker:should_send(none, none)),
        ?_assertEqual(false, license_notice_worker:should_send(none, 7)),
        %% 与上次已发档相同：不重发
        ?_assertEqual(false, license_notice_worker:should_send(7, 7)),
        ?_assertEqual(false, license_notice_worker:should_send(expired, expired)),
        %% 进入新档（更紧急或首次）：发
        ?_assertEqual(true, license_notice_worker:should_send(7, none)),
        ?_assertEqual(true, license_notice_worker:should_send(7, 30)),
        ?_assertEqual(true, license_notice_worker:should_send(1, 7)),
        ?_assertEqual(true, license_notice_worker:should_send(expired, 1))
    ].
