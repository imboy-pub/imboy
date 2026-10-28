%% IMBOY_ERLANG_QUALITY P1-03：结果落盘（原子写 + sha256，UTF-8 JSON）。
%% 生成终结消息仅在结果原子落盘之后（FRAMEWORK_DESIGN §7）。
-module(imboy_quality_report).
-export([write/2, summarize/1]).

write(Path, Result) when is_map(Result) ->
    Bytes = iolist_to_binary(json:encode(Result)),
    Temp = Path ++ ".tmp." ++ os:getpid(),
    {ok, H} = file:open(Temp, [write, binary, raw, exclusive]),
    try
        ok = file:write(H, Bytes),
        ok = file:sync(H)
    after
        ok = file:close(H)
    end,
    ok = file:rename(Temp, Path),
    {ok, #{
        path => Path,
        bytes => byte_size(Bytes),
        sha256 => crypto:hash(sha256, Bytes)
    }}.

summarize(#{exit := Exit, wall_ms := Wall, counts := Counts}) ->
    unicode:characters_to_binary(
        io_lib:format(
            "exit=~tp wall_ms=~tp pass=~tp fail=~tp skip=~tp cancel=~tp",
            [
                Exit,
                Wall,
                maps:get(pass, Counts),
                maps:get(fail, Counts),
                maps:get(skip, Counts),
                maps:get(cancel, Counts)
            ]
        )
    ).
