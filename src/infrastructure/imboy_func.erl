-module (imboy_func).

-export ([is_mobile/1, milliseconds/0]).

-spec is_mobile(Mobile :: list()) -> true | false.

%% 得到复现在制时间毫秒
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
