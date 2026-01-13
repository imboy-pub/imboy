-module(elib_email_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_email 模块的 EUnit 测试
%%%
%%% 目标：验证邮件发送功能
%%% 覆盖：仅主题发送、带正文发送、配置处理
%%%===================================================================

%% ===================================================================
%% send/2 测试 - 仅主题发送
%% ===================================================================

send_with_subject_only_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [{username, "test@example.com"}, {relay, "smtp.example.com"}]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded_email">> end}
        ]},
        {ec_cnv, [
            {'to_binary', 1, fun(Input) -> Input end}
        ]}
    ], fun() ->
        Subject = <<"测试主题"/utf8>>,
        Result = elib_email:send(<<"recipient@example.com">>, Subject),
        ?assertEqual({ok, success}, Result)
    end).

send_with_list_subject_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [{username, "test@example.com"}]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]},
        {ec_cnv, [
            {'to_binary', 1, fun(_Input) -> <<"converted"/utf8>> end}
        ]}
    ], fun() ->
        Result = elib_email:send(<<"recipient@example.com">>, "列表主题"),
        ?assertEqual({ok, success}, Result)
    end).

send_with_binary_subject_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [{username, <<"sender@example.com">>}]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"二进制主题"/utf8>>,
        Result = elib_email:send(<<"to@example.com">>, Subject),
        ?assertEqual({ok, success}, Result)
    end).

%% ===================================================================
%% send/3 测试 - 带正文发送
%% ===================================================================

send_with_subject_and_body_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [{username, "sender@example.com"}]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun({From, [To], _Encoded}, _Option) ->
                ?assertEqual(<<"sender@example.com">>, From),
                ?assertEqual(<<"to@example.com">>, To),
                ok
            end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"mime_encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"邮件主题"/utf8>>,
        Body = <<"邮件正文内容"/utf8>>,
        Result = elib_email:send(<<"to@example.com">>, Subject, Body),
        ?assertEqual({ok, success}, Result)
    end).

send_with_empty_body_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) -> [{username, "test"}] end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"主题"/utf8>>,
        Result = elib_email:send(<<"to@example.com">>, Subject, <<>>),
        ?assertEqual({ok, success}, Result)
    end).

send_with_long_body_test_() ->
    LongBody = list_to_binary(lists:duplicate(1000, $x)),
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) -> [{username, "test"}] end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"主题"/utf8>>,
        Result = elib_email:send(<<"to@example.com">>, Subject, LongBody),
        ?assertEqual({ok, success}, Result)
    end).

%% ===================================================================
%% 配置处理测试
%% ===================================================================

send_with_valid_smtp_option_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [
                    {username, "user@test.com"},
                    {relay, "smtp.test.com"},
                    {port, 587}
                ]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, Option) ->
                ?assertMatch([{_, _}, {_, _}, {_, _}], Option),
                ok
            end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"主题"/utf8>>,
        Body = <<"正文"/utf8>>,
        Result = elib_email:send(<<"to@test.com">>, Subject, Body),
        ?assertEqual({ok, success}, Result)
    end).

send_with_empty_smtp_option_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) -> [] end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"主题"/utf8>>,
        Result = elib_email:send(<<"to@test.com">>, Subject),
        ?assertEqual({ok, success}, Result)
    end).

send_without_username_in_option_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [{relay, "smtp.test.com"}]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"主题"/utf8>>,
        Result = elib_email:send(<<"to@test.com">>, Subject),
        ?assertEqual({ok, success}, Result)
    end).

send_with_atom_username_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [{username, 'test@example.com'}]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]},
        {ec_cnv, [
            {'to_binary', 1, fun(Input) ->
                ?assertEqual('test@example.com', Input),
                <<"test@example.com">>
            end}
        ]}
    ], fun() ->
        Subject = <<"主题"/utf8>>,
        Result = elib_email:send(<<"to@test.com">>, Subject),
        ?assertEqual({ok, success}, Result)
    end).

%% ===================================================================
%% 邮件结构测试
%% ===================================================================

send_validates_email_structure_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [{username, "sender@example.com"}]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun({_From, [To], _Encoded}, _Option) ->
                ?assertEqual(<<"recipient@example.com">>, To),
                ok
            end}
        ]},
        {mimemail, [
            {'encode', 1, fun(Email) ->
                case Email of
                    {<<"text">>, <<"html">>, EmailHeaders, _Params, _Body} ->
                        ?assertMatch({<<"From">>, _}, lists:keyfind(<<"From">>, 1, EmailHeaders)),
                        ?assertMatch({<<"To">>, _}, lists:keyfind(<<"To">>, 1, EmailHeaders)),
                        ?assertMatch({<<"Subject">>, _}, lists:keyfind(<<"Subject">>, 1, EmailHeaders));
                    _ ->
                        ?assert(false, invalid_email_structure)
                end,
                <<"encoded">>
            end}
        ]}
    ], fun() ->
        Subject = <<"测试主题"/utf8>>,
        Body = <<"测试正文"/utf8>>,
        Result = elib_email:send(<<"recipient@example.com">>, Subject, Body),
        ?assertEqual({ok, success}, Result)
    end).

send_validates_utf8_charset_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) ->
                [{username, "sender@example.com"}]
            end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun({_Type, _SubType, _Headers, Params, _Body}) ->
                ?assertMatch(
                    #{content_type_params := [{<<"charset">>, <<"utf-8">>}],
                      disposition := <<"inline">>},
                    Params
                ),
                <<"encoded">>
            end}
        ]}
    ], fun() ->
        Subject = <<"中文主题"/utf8>>,
        Body = <<"中文正文"/utf8>>,
        Result = elib_email:send(<<"to@example.com">>, Subject, Body),
        ?assertEqual({ok, success}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

send_with_special_characters_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) -> [{username, "test"}] end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        SpecialSubject = <<"主题!@#$%^&*()">>,
        SpecialBody = <<"正文<>{}[]|\\\"'">>,
        Result = elib_email:send(<<"to@example.com">>, SpecialSubject, SpecialBody),
        ?assertEqual({ok, success}, Result)
    end).

send_with_mixed_utf8_content_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) -> [{username, "test"}] end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"Subject Test Chinese"/utf8>>,
        Body = <<"Body Content 内容"/utf8>>,
        Result = elib_email:send(<<"to@example.com">>, Subject, Body),
        ?assertEqual({ok, success}, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

send_returns_ok_success_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(smtp_option) -> [{username, "test"}] end}
        ]},
        {gen_smtp_client, [
            {'send', 2, fun(_Email, _Option) -> ok end}
        ]},
        {mimemail, [
            {'encode', 1, fun(_Email) -> <<"encoded">> end}
        ]}
    ], fun() ->
        Subject = <<"主题"/utf8>>,
        Result = elib_email:send(<<"to@example.com">>, Subject),
        ?assertMatch({ok, success}, Result)
    end).
