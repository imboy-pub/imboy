-module(elib_multipart).
%%%
% 流式 multipart/form-data 收集器（POST /api/v1/attachment/upload 用）
%
% 目标：把 multipart 请求体中 name="file" 的 part 的 body 以小块流式写入
% 调用方提供的写回调（临时文件句柄），全程不把整个文件驻留内存。
% 解析复用依赖库 cowlib 的 cow_multipart（parse_headers/parse_body 二元组
% 状态机），本模块只做分块驱动 + part 选择 + 大小上限。
%
% 用法（handler 侧典型循环）：
%   St0 = elib_multipart:new(Boundary, WriteFun, MaxSize),
%   Loop: elib_multipart:stream(Data, St) -> {more, St} | {done, St} | {error, R}
%   结束: elib_multipart:result(St) -> {ok, #{size => N}} | {error, no_file_part}
%
% stream/2 语义：
%   {more, St}   还需要更多数据（继续 cowboy_req:read_body 再喂）
%   {done, St}   收到 closing boundary，整个 multipart 解析完成
%   {error, R}   file_too_large（超 MaxSize）/ {bad_part, term()}（协议错）
%
% 请求体读完但未收到 closing boundary 时由调用方判 incomplete_body
% （stream 返回 {more,_} 而请求体已尽）。
%%%

-export([new/3, stream/2, result/1]).

-export_type([state/0]).

-type write_fun() :: fun((binary()) -> ok).

-type reason() :: file_too_large | {bad_part, term()} | {write_failed, term()}.

-opaque state() :: #{
    boundary := binary(),
    write := write_fun(),
    max_size := non_neg_integer(),
    buf := binary(),
    phase := headers | body | done,
    %% 当前 part 的 form 字段名（来自 Content-Disposition）
    part_name := binary() | undefined,
    %% 是否见过 name="file" 的 part
    file_seen := boolean(),
    %% file part 已累计写入字节数
    file_size := non_neg_integer(),
    %% 请求体原始字节累计（含被丢弃的非 file part），总量上限用
    raw_seen := non_neg_integer()
}.

%% multipart 头部区（boundary 行 + part headers）通常很小；恶意超大头部
%% 在此截断（parse_headers 一直 more 的 buf 上限），1MB 足够容纳文件名。
-define(MAX_HEADERS_BUF, 1024 * 1024).

-spec new(binary(), write_fun(), non_neg_integer()) -> state().
new(Boundary, WriteFun, MaxSize) when
    is_binary(Boundary), is_function(WriteFun, 1), is_integer(MaxSize), MaxSize > 0
->
    #{
        boundary => Boundary,
        write => WriteFun,
        max_size => MaxSize,
        buf => <<>>,
        phase => headers,
        part_name => undefined,
        file_seen => false,
        file_size => 0,
        raw_seen => 0
    }.

-spec stream(binary(), state()) -> {more, state()} | {done, state()} | {error, reason()}.
stream(Data, St0) when is_binary(Data) ->
    St = St0#{buf => <<(maps:get(buf, St0))/binary, Data/binary>>},
    try
        drive(St)
    catch
        %% WriteFun 故障（磁盘满/IO 错误）单独分流，调用方映射 5xx
        throw:{write_failed, _} = WErr ->
            {error, WErr};
        %% cow_multipart 对非法结构可能抛 function_clause/badarg
        Class:R when Class =:= error; Class =:= exit ->
            {error, {bad_part, R}}
    end.

%% @doc 解析结果。仅在 stream 返回 {done, St} 后调用。
-spec result(state()) -> {ok, #{size := non_neg_integer()}} | {error, no_file_part}.
result(#{phase := done, file_seen := true, file_size := Size}) ->
    {ok, #{size => Size}};
result(_St) ->
    {error, no_file_part}.

%% ===================================================================
%% 状态机驱动：直到 hold（缺数据）/ stop（done）或错误
%% ===================================================================

drive(St) ->
    case step(St) of
        {hold, St2} -> {more, St2};
        {stop, St2} -> {done, St2};
        {next, St2} -> drive(St2);
        {error, _} = E -> E
    end.

step(#{phase := headers, buf := Buf, boundary := Boundary} = St) ->
    case cow_multipart:parse_headers(Buf, Boundary) of
        more ->
            case byte_size(Buf) > ?MAX_HEADERS_BUF of
                true -> {stop, St#{phase := done}};
                false -> {hold, St}
            end;
        {more, Buf2} ->
            {hold, St#{buf := Buf2}};
        {ok, Headers, Rest} ->
            {next, St#{phase := body, part_name => part_name(Headers), buf := Rest}};
        {done, _Epilogue} ->
            {stop, St#{phase := done, buf := <<>>, part_name := undefined}}
    end;
step(#{phase := body, buf := <<>>} = St) ->
    %% buf 空时 parse_body 会返回 {ok, <<>>} 造成空转，必须 hold 等数据
    {hold, St};
step(#{phase := body, buf := Buf, boundary := Boundary} = St) ->
    case cow_multipart:parse_body(Buf, Boundary) of
        {ok, Data} ->
            %% 未到 part 末尾；Buf 全部是安全可写 body（尾部无部分 boundary）
            write_body(Data, St#{buf := <<>>});
        {ok, Data, Rest} ->
            %% Rest == 原 Buf 且无数据可写：buf 尾部全是疑似部分 boundary，
            %% 必须等更多数据（否则同 buf 重解析造成空转）
            case Data =:= <<>> andalso Rest =:= Buf of
                true -> {hold, St};
                false -> write_body(Data, St#{buf := Rest})
            end;
        done ->
            {next, St#{phase := headers, buf := <<>>, part_name := undefined}};
        {done, Data} ->
            part_end(write_body(Data, St#{buf := <<>>}));
        {done, Data, Rest} ->
            part_end(write_body(Data, St#{buf := Rest}))
    end;
step(#{phase := done} = St) ->
    {stop, St}.

%% part 结束后回到 headers 相位等待下一个 part / closing boundary。
part_end({next, St}) ->
    {next, St#{phase := headers, part_name := undefined}};
part_end({error, _} = E) ->
    E.

%% 只写 name="file" 的 part（其他 form 字段丢弃）；累计大小超限即停。
write_body(<<>>, St) ->
    {next, St};
write_body(Data, St) ->
    %% 请求体原始字节不分 part 一并计数：非 file 字段的数据虽被丢弃，
    %% 也必须在 max_size 内，防止已认证用户用超大垃圾字段放大带宽/CPU。
    Raw = maps:get(raw_seen, St) + byte_size(Data),
    case Raw > maps:get(max_size, St) of
        true ->
            {error, file_too_large};
        false ->
            write_selected(Data, St#{raw_seen := Raw})
    end.

%% file part：写回调 + 独立计数（file_size 上限 = max_size）；非 file part 丢弃。
write_selected(Data, #{part_name := <<"file">>, write := Write} = St) ->
    Size = maps:get(file_size, St) + byte_size(Data),
    case Size > maps:get(max_size, St) of
        true ->
            {error, file_too_large};
        false ->
            %% WriteFun 故障（磁盘满/IO 错误）与协议解析错误分流：
            %% 前者是服务端故障应映射 5xx，不能被笼统归为 bad_part 400。
            try Write(Data) of
                ok ->
                    {next, St#{file_size := Size, file_seen := true}};
                Unexpected ->
                    %% WriteFun 返回非 ok（契约违例）同样按写失败处理
                    erlang:throw({write_failed, {bad_return, Unexpected}})
            catch
                C:R ->
                    erlang:throw({write_failed, {'EXIT', {C, R}}})
            end
    end;
write_selected(_Data, St) ->
    {next, St}.

%% 从 part headers 提取 Content-Disposition 的 name 参数。
-spec part_name([{binary(), binary()}]) -> binary() | undefined.
part_name(Headers) ->
    case lists:keyfind(<<"content-disposition">>, 1, Headers) of
        {_, Value} ->
            {_Type, Params} = cow_multipart:parse_content_disposition(Value),
            case lists:keyfind(<<"name">>, 1, Params) of
                {_, Name} -> Name;
                false -> undefined
            end;
        false ->
            undefined
    end.
