-module (func).

-export ([is_mobile/1]).
-export ([num_random/1]).

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

%% 生成Len位随机数
num_random(Len) ->
    Prefix = rand:uniform(9),
    MinNum = round(math:pow(10, Len - 1)),
    Num = rand:uniform(MinNum),
    % ?LOG([MinNum]),
    if
        Num > MinNum ->
            Num;
        true ->
            MinNum * Prefix + Num
    end.
