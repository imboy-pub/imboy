-module(cowboy_req_h).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% Cowboy 请求模拟器
%%%
%%% 用于在单元测试中模拟 cowboy_req 行为
%%% 提供 HTTP 请求和响应的模拟功能
%%%===================================================================

-export([
    new/1,
    response/1,
    set_response/3,
    method/1,
    uri/1,
    qs/1,
    headers/1,
    body/1,
    match_qs/2
]).

%% @doc 创建一个模拟的 cowboy 请求
%% @param Options 请求选项映射
%% @returns 模拟请求对象
new(Options) when is_map(Options) ->
    Default = #{
        method => <<"GET">>,
        uri => <<"/">>,
        qs => <<>>,
        headers => #{},
        body => <<>>,
        bindings => #{},
        peer => {{127, 0, 0, 1}, 12345},
        cert => undefined,
        response_status => undefined,
        response_headers => #{},
        response_body => undefined
    },
    maps:merge(Default, Options).

%% @doc 获取模拟请求的响应信息
%% @param Req 模拟请求对象
%% @returns {StatusCode, Headers, Body}
response(Req) ->
    Status = maps:get(response_status, Req, 200),
    Headers = maps:get(response_headers, Req, #{}),
    Body = maps:get(response_body, Req, <<>>),
    {Status, Headers, Body}.

%% @doc 设置模拟请求的响应
%% @param Req 模拟请求对象
%% @param StatusCode HTTP状态码
%% @param Body 响应体
%% @returns 更新后的请求对象
set_response(Req, StatusCode, Body) ->
    Req2 = Req#{response_status => StatusCode},
    Req2#{response_body => Body}.

%% @doc 获取请求方法
%% @param Req 模拟请求对象
%% @returns HTTP方法
method(Req) ->
    maps:get(method, Req).

%% @doc 获取请求URI
%% @param Req 模拟请求对象
%% @returns 请求URI
uri(Req) ->
    maps:get(uri, Req).

%% @doc 获取查询字符串
%% @param Req 模拟请求对象
%% @returns 查询字符串
qs(Req) ->
    maps:get(qs, Req).

%% @doc 获取请求头
%% @param Req 模拟请求对象
%% @returns 请求头映射
headers(Req) ->
    maps:get(headers, Req).

%% @doc 获取请求体
%% @param Req 模拟请求对象
%% @returns 请求体
body(Req) ->
    maps:get(body, Req).

%% @doc 匹配查询字符串参数
%% @param Fields 字段列表，格式为 [{Key, [], Default}]
%% @param Req 模拟请求对象
%% @returns 包含匹配结果的映射
match_qs(Fields, Req) when is_list(Fields) ->
    Qs = maps:get(qs, Req, <<>>),
    parse_qs_fields(Fields, Qs, #{}).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 解析查询字符串字段
%% @param Fields 字段列表
%% @param Qs 查询字符串
%% @param Acc 累加器
%% @returns 包含解析结果的映射
parse_qs_fields([], _Qs, Acc) ->
    Acc;
parse_qs_fields([{Key, [], Default} | Rest], Qs, Acc) ->
    Value = parse_qs_value(Key, Qs, Default),
    parse_qs_fields(Rest, Qs, Acc#{Key => Value}).

%% @doc 从查询字符串中解析指定键的值
%% @param Key 键
%% @param Qs 查询字符串
%% @param Default 默认值
%% @returns 解析后的值
parse_qs_value(Key, Qs, Default) ->
    KeyBin = ec_cnv:to_binary(Key),
    case binary:split(Qs, <<"&">>, [global]) of
        [] ->
            Default;
        Parts ->
            case lists:filter(fun(Part) ->
                case binary:split(Part, <<"=">>) of
                    [K, _] -> K =:= KeyBin;
                    _ -> false
                end
            end, Parts) of
                [] ->
                    Default;
                [Match | _] ->
                    case binary:split(Match, <<"=">>) of
                        [_, Value] -> Value;
                        _ -> Default
                    end
            end
    end.

%% ===================================================================
%% EUnit 测试
%% ===================================================================

-ifdef(EUNIT).

new_test_() ->
    [
        ?_assertEqual(<<"GET">>, method(new(#{}))),
        ?_assertEqual(<<"/test">>, uri(new(#{uri => <<"/test">>}))),
        ?_assertEqual(<<"key=value">>, qs(new(#{qs => <<"key=value">>}))),
        ?_assertEqual(#{}, headers(new(#{}))),
        ?_assertEqual(<<"test body">>, body(new(#{body => <<"test body">>})))
    ].

response_test_() ->
    Req = new(#{
        response_status => 404,
        response_headers => #{<<"content-type">> => <<"text/html">>},
        response_body => <<"Not Found">>
    }),
    [
        ?_assertEqual({404, #{<<"content-type">> => <<"text/html">>}, <<"Not Found">>}, 
                     response(Req))
    ].

set_response_test_() ->
    Req = new(#{}),
    Req2 = set_response(Req, 201, <<"Created">>),
    ?_assertEqual({201, #{}, <<"Created">>}, response(Req2)).

-endif.