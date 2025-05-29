-module(imboy_func).

-include_lib("imlib/include/log.hrl").

-export([uid/0, uid/1]).
-export([generate_session_id/0]).
-export([is_mobile/1]).
-export([is_email/1]).
-export([num_random/1]).
-export([send_email/2]).
-export([send_email/3]).
-export([is_proplist/1]).


% generate a session id string
% imboy_func:generate_session_id().
% 1700_475816_502993
-spec generate_session_id() -> list().
generate_session_id() ->
    {T1, T2, T3} = erlang:timestamp(),
    lists:flatten(io_lib:format("~p_~p_~p", [T1, T2, T3])).

-spec uid() -> binary().
uid() ->
    uid("").

-spec uid(integer() | list() | binary()) -> binary().
uid(Prefix) ->
    U1 = uid:encode64(uid:g()),
    iolist_to_binary([ec_cnv:to_binary(Prefix), U1]).

-spec is_mobile(Mobile :: list()) -> true | false.
is_mobile(Mobile) ->
    {_, P} = re:compile("^1[0-9]{10}$"),
    case re:run(Mobile, P) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.


%% imboy_func:is_email(Email)
-spec is_email(Email :: list()) -> true | false.
is_email(Email) ->
    {_, P} = re:compile("^[a-zA-Z0-9_-]+@[a-zA-Z0-9_-]+(\.[a-zA-Z0-9_-]+)+$"),
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
    % ?DEBUG_LOG([MinNum]),
    case Num > MinNum of
        true ->
            Num;
        _ ->
            MinNum * Prefix + Num
    end.


% imboy_func:send_email(<<"leeyisoft@icloud.com">>, <<"你的验证码为： 12345，10分钟后过期。"/utf8>>).
-spec send_email(binary(), binary()) -> {ok, pid()}.
send_email(ToEmail, Subject) when is_list(Subject) ->
    send_email(ToEmail, ec_cnv:to_binary(Subject));
send_email(ToEmail, Subject) ->
    send_email(ToEmail, Subject, <<>>).

send_email(ToEmail, Subject, Body) ->
    Option = config_ds:env(smtp_option),
    Username = proplists:get_value(username, Option),
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

    gen_smtp_client:send({
        Username2,
        [ToEmail],
        mimemail:encode(Email)
    }, Option),
    {ok, success}.

% gen_smtp_client:send({Username,
%                       [binary_to_list(ToEmail)],
%                       "Subject: " ++ Subject},
%                      Option).


is_proplist(Var) ->
    is_list(Var) andalso lists:all(fun({_, _}) -> true; (_) -> false end, Var).
