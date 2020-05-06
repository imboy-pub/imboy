-module (imboy_func).

-export ([milliseconds/0]).
-export ([is_mobile/1]).
-export ([md5/1]).

-spec is_mobile(Mobile :: list()) -> true | false.

%% 得到现在在制时间毫秒
milliseconds() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    1000000000 * MegaSecs + Secs * 1000 + MicroSecs div 1000.

is_mobile(Mobile) ->
    {_, P} = re:compile("^1[0-9]{10}$"),
    case re:run(Mobile, P) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.

%% erlang md5 16进制字符串
md5(Str) ->
    Sig = erlang:md5(Str),
    iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)]).
