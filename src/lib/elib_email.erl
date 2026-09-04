-module(elib_email).

%%% @doc 通用函数工具模块
%%% 提供邮件发送等功能

-include("log.hrl").

-export([send/2]).
-export([send/3]).

%% @doc 发送邮件，只有主题
%% @param ToEmail 收件人邮箱地址
%% @param Subject 邮件主题
%% @returns {ok, success}
%% 示例: elib_email:send(<<"leeyisoft@icloud.com">>, <<"你的验证码为： 12345，10分钟后过期。"/utf8>>)
-spec send(binary(), binary() | list()) -> {ok, success} | {error, binary()}.
send(ToEmail, Subject) when is_list(Subject) ->
    send(ToEmail, ec_cnv:to_binary(Subject));
send(ToEmail, Subject) ->
    send(ToEmail, Subject, <<>>).

%% @doc 发送邮件，包含主题和正文
%% @param ToEmail 收件人邮箱地址
%% @param Subject 邮件主题
%% @param Body 邮件正文
%% @returns {ok, success} | {error, binary()}
-spec send(binary(), binary(), binary()) -> {ok, success} | {error, binary()}.
send(ToEmail, Subject, Body) ->
    Option = config_ds:env(smtp_option),
    Username =
        case Option of
            L when is_list(L) ->
                case lists:keyfind(username, 1, L) of
                    {_, U} -> U;
                    false -> undefined
                end;
            _ ->
                undefined
        end,
    Username2 = ec_cnv:to_binary(Username),
    From =
        case Option of
            L2 when is_list(L2) ->
                case lists:keyfind(from, 1, L2) of
                    {_, FromValue} -> ec_cnv:to_binary(FromValue);
                    false -> Username2
                end;
            _ ->
                Username2
        end,

    %% RFC5322 校验：From 地址必须是合法 email 格式
    case is_valid_email(From) of
        false ->
            ?ERROR_LOG({smtp_config_error, invalid_from_address, From}),
            {error, <<"SMTP From 地址未配置或格式无效"/utf8>>};
        true ->
            ClientOption = proplists:delete(from, Option),
            Email = {
                <<"text">>,
                <<"html">>,
                [
                    {<<"From">>, From},
                    {<<"To">>, ToEmail},
                    {<<"Subject">>, Subject}
                ],
                #{
                    content_type_params => [
                        {<<"charset">>, <<"utf-8">>}
                    ],
                    disposition => <<"inline">>
                },
                Body
            },
            try mimemail:encode(Email) of
                Encoded ->
                    _ = gen_smtp_client:send(
                        {
                            Username2,
                            [ToEmail],
                            Encoded
                        },
                        ClientOption
                    ),
                    {ok, success}
            catch
                Class:Reason:Stacktrace ->
                    ?ERROR_LOG({smtp_encode_error, Class, Reason, Stacktrace}),
                    {error, <<"邮件编码失败"/utf8>>}
            end
    end.

%% @doc 校验 email 地址基本格式（包含 @ 且非空）
-spec is_valid_email(binary()) -> boolean().
is_valid_email(Addr) when is_binary(Addr), byte_size(Addr) > 3 ->
    case binary:match(Addr, <<"@">>) of
        nomatch -> false;
        {Pos, 1} when Pos > 0, Pos < byte_size(Addr) - 1 -> true;
        _ -> false
    end;
is_valid_email(_) ->
    false.

% gen_smtp_client:send({Username,
%                       [binary_to_list(ToEmail)],
%                       "Subject: " ++ Subject},
%                      Option).
