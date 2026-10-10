-module(org_invite_landing_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% APP-M02：组织邀请 H5 落地页纯函数合同测试
%% （形状门与生成器同口径；HTML 合同含深链 payload；无效页零反射）
%% ===================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%% 生成器字母表（organization_invite_code_pg:?INVITE_CHARSET）内的 8 位码
%% 必须全部放行——否则真实邀请码会被落地页 404。
valid_code_accepts_invite_charset_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            ?assert(org_invite_landing_handler:valid_code(<<"AB2C3D4E">>)),
            ?assert(org_invite_landing_handler:valid_code(<<"ZZZZZZ99">>)),
            %% 边界字符：K/W/Y 在集内（I/O 除外）
            ?assert(org_invite_landing_handler:valid_code(<<"KWKWKWKY">>)),
            ?assert(org_invite_landing_handler:valid_code(<<"23456789">>))
        end
    end}.

valid_code_rejects_lookalike_and_bad_shape_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            %% 手抄混淆字符（生成器排除的 0/O/1/I）
            ?assertNot(org_invite_landing_handler:valid_code(<<"AB2C3D40">>)),
            ?assertNot(org_invite_landing_handler:valid_code(<<"AB2C3D4I">>)),
            ?assertNot(org_invite_landing_handler:valid_code(<<"AB2C3D4O">>)),
            ?assertNot(org_invite_landing_handler:valid_code(<<"AB2C3D41">>)),
            %% 形状：小写 / 长度 / 空 / 非二进制
            ?assertNot(org_invite_landing_handler:valid_code(<<"ab2c3d4e">>)),
            ?assertNot(org_invite_landing_handler:valid_code(<<"AB2C3D4">>)),
            ?assertNot(org_invite_landing_handler:valid_code(<<"AB2C3D4EX">>)),
            ?assertNot(org_invite_landing_handler:valid_code(<<>>)),
            ?assertNot(org_invite_landing_handler:valid_code(12345678))
        end
    end}.

%% HTML 合同：码出现、深链 payload 与 App 端 parseOrgJoinDeepLink 契约一致、
%% 主路径（复制）与加入路径（加入企业）文案在位。
landing_html_contains_contract_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Html = org_invite_landing_handler:landing_html(<<"AB2C3D4E">>),
            ?assert(nomatch =/= binary:match(Html, <<"AB2C3D4E">>)),
            ?assert(
                nomatch =/= binary:match(Html, <<"imboy://org/join?c=AB2C3D4E">>)
            ),
            ?assert(nomatch =/= binary:match(Html, <<"复制邀请码"/utf8>>)),
            ?assert(nomatch =/= binary:match(Html, <<"加入企业"/utf8>>)),
            %% CSP 收紧：零外部资源（default-src 'none'），内联样式/脚本放行
            ?assert(
                nomatch =/= binary:match(Html, <<"copyCode()">>)
            )
        end
    end}.

%% 404 无效页完全静态：不含脚本、不含任何占位输入（零反射面）。
invalid_html_static_and_no_reflection_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Html = org_invite_landing_handler:invalid_html(),
            ?assert(
                nomatch =/= binary:match(Html, <<"邀请链接无效"/utf8>>)
            ),
            ?assertEqual(nomatch, binary:match(Html, <<"<script">>)),
            ?assertEqual(nomatch, binary:match(Html, <<"imboy://">>))
        end
    end}.
