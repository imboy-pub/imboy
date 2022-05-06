-module(aas_friend_infomaction).
%%%
% aas_friend_infomaction 是 friend_infomaction application assembler 缩写
%%%

-export([data/5]).


data(CurrentUid, Type, User, UserSetting, Friend) ->
    lists:append([[{<<"mine_uid">>,
                    hashids_translator:uid_encode(CurrentUid)},
                   {<<"type">>, Type}],
                  hashids_translator:replace_id(User),
                  UserSetting,
                  Friend]).
% [
%     {<<"account">>, <<"xxxxx">>},
%     {<<"nickname">>, <<"xxxxx">>},
%     {<<"avatar">>, <<"/static/image/user_default_avatar.jpeg">>},
%     {<<"gender">>, <<"3">>},
%     {<<"birthday">>, <<"2020-05-23">>},
%     {<<"blood_type">>, <<"A型">>},
%     {<<"job">>, <<"1">>},
%     {<<"qq">>, <<"">>},
%     {<<"wechat">>, <<"">>},
%     {<<"mobile">>, <<"">>},
%     {<<"email">>, <<"">>},
%     {<<"sign">>, <<"">>},
% ].
