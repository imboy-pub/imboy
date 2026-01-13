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
-spec send(binary(), binary() | list()) -> {ok, success}.
send(ToEmail, Subject) when is_list(Subject) ->
    send(ToEmail, ec_cnv:to_binary(Subject));
send(ToEmail, Subject) ->
    send(ToEmail, Subject, <<>>).

%% @doc 发送邮件，包含主题和正文
%% @param ToEmail 收件人邮箱地址
%% @param Subject 邮件主题
%% @param Body 邮件正文
%% @returns {ok, success}
-spec send(binary(), binary(), binary()) -> {ok, success}.
send(ToEmail, Subject, Body) ->
    Option = config_ds:env(smtp_option),
    Username = case Option of
        L when is_list(L) ->
            case lists:keyfind(username, 1, L) of
                {_, U} -> U;
                false -> undefined
            end;
        _ ->
            undefined
    end,
    Username2 = ec_cnv:to_binary(Username),

    Email = {
        <<"text">>,
        <<"html">>,
        [
            {<<"From">>, Username2},
            {<<"To">>, ToEmail},
            {<<"Subject">>, Subject}
        ],
        #{content_type_params => [
            {<<"charset">>, <<"utf-8">>}],
            disposition => <<"inline">>
        } ,
        Body
    },

    _ = gen_smtp_client:send({
        Username2,
        [ToEmail],
        mimemail:encode(Email)
    }, Option),
    {ok, success}.

% gen_smtp_client:send({Username,
%                       [binary_to_list(ToEmail)],
%                       "Subject: " ++ Subject},
%                      Option).
