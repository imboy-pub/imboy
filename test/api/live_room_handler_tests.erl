-module(live_room_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% live_room_handler：list/my_list 响应键口径回归。
% 背景（fix 0e1de5b5）：page_with_total 返回 atom 键（list/total/page/size），
% handler 曾用 binary 键 maps:get(<<"list">>, ...) / #{<<"list">> => ...} 读写，
% 同一 map 并存 atom `list`（真实数据）与 binary `<<"list">>`（空数组），
% JSON 编码产出重复 "list" 键，客户端 jsonDecode 保留后者——房间列表永远为空。
% 本套件锁死：响应 map 只允许 atom list 键且值为 encode_room 后的行。
%%%

rows() ->
    [
        #{
            id => 111596538331138048,
            title => <<"AT-LIVE-FIXTURE">>,
            status => 0,
            viewer_count => 5,
            cover => <<>>,
            user_id => 1000000056
        }
    ].

resp_msgs() ->
    [
        {'success', 3, fun(_Req, Data, _Msg) ->
            self() ! {resp_data, Data},
            req_ok
        end},
        {'error', 2, fun(_Req, _Msg) -> req_error end}
    ].

recv_data() ->
    receive
        {resp_data, D} -> D
    after 100 -> timeout
    end.

%% my_list：单 atom list 键、无 binary 键共存、行原样透出
my_list_single_atom_list_key_test_() ->
    Rows = rows(),
    ?WITH_MECKS(
        [
            {auth_ds, [{'current_uid', 1, fun(_State) -> 1000000056 end}]},
            {elib_param, [{'page', 1, fun(_Req) -> {1, 20} end}]},
            {live_room_logic, [
                {'page_by_uid', 3, fun(Uid, Page, Size) ->
                    ?assertEqual(1000000056, Uid),
                    ?assertEqual(1, Page),
                    ?assertEqual(20, Size),
                    {ok, #{total => 1, page => Page, size => Size, list => Rows}}
                end}
            ]},
            {elib_response, resp_msgs()}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, _Req, _State} = live_room_handler:init(MockReq, #{action => my_list}),
            Data = recv_data(),
            ?assert(maps:is_key(list, Data)),
            ?assertNot(maps:is_key(<<"list">>, Data)),
            ?assertEqual(Rows, maps:get(list, Data)),
            ?assertEqual(1, maps:get(total, Data))
        end
    ).

%% list（公开列表）：同口径断言；空列表时 list 仍为键、值 []
list_single_atom_list_key_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [{'page', 1, fun(_Req) -> {1, 20} end}]},
            {live_room_logic, [
                {'page_active', 2, fun(Page, Size) ->
                    ?assertEqual(1, Page),
                    ?assertEqual(20, Size),
                    {ok, #{total => 0, page => Page, size => Size, list => []}}
                end}
            ]},
            {elib_response, resp_msgs()}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, _Req, _State} = live_room_handler:init(MockReq, #{action => list}),
            Data = recv_data(),
            ?assert(maps:is_key(list, Data)),
            ?assertNot(maps:is_key(<<"list">>, Data)),
            ?assertEqual([], maps:get(list, Data)),
            ?assertEqual(0, maps:get(total, Data))
        end
    ).
