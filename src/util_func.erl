-module(util_func).

-export([is_mobile/1]).
-export([is_email/1]).
-export([num_random/1]).
-export([send_email/2]).

-include("common.hrl").


-spec is_mobile(Mobile :: list()) -> true | false.
is_mobile(Mobile) ->
    {_, P} = re:compile("^1[0-9]{10}$"),
    case re:run(Mobile, P) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.


-spec is_email(Email :: list()) -> true | false.
is_email(Email) ->
    {_, P} =
        re:compile("^[a-zA-Z0-9_-]+@[a-zA-Z0-9_-]+(\.[a-zA-Z0-9_-]+)+$"),
    case re:run(Email, P) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.


%% 生成Len位随机数
num_random(Len) ->
    Prefix = rand:uniform(9),
    MinNum = round(math:pow(10, Len - 1)),
    Num = rand:uniform(MinNum),
    % ?LOG([MinNum]),
    case Num > MinNum of
        true ->
            Num;
        _ ->
            MinNum * Prefix + Num
    end.


% util_func:send_email(<<"1977699124@qq.com">>,
%    "code is: " ++ integer_to_list(util_func:num_random(6)) ++
%    " , will expire in 10 minutes.").
% util_func:send_email(<<"leeyisoft@icloud.com">>,
%   "code is: " ++ integer_to_list(util_func:num_random(6)) ++
%   " , will expire in 10 minutes.").
% 中文支持，TODO
% util_func:send_email(<<"leeyisoft@icloud.com">>,
%   "你的验证码为： " ++ integer_to_list(util_func:num_random(6)) ++
%   " ，10分钟后过期。").
% util_func:send_email(<<"1977699124@qq.com">>, "你的验证码为： " ++
%   integer_to_list(util_func:num_random(6)) ++ " ，10分钟后过期。").
-spec send_email(ToEmail :: binary(), Subject :: list()) -> {ok, pid()}.
send_email(ToEmail, Subject) ->
    {ok, Option} = application:get_env(imboy, smtp_option),
    Username = proplists:get_value(username, Option),
    gen_smtp_client:send({Username,
                          [binary_to_list(ToEmail)],
                          "Subject: " ++ Subject},
                         Option).
