
-define(APP, imboy).

% token有效期 2小时 单位秒 7200000 = 3600 * 2
-define(TOKEN_VALID, 7200).
% refreshtoken有效期 365天 单位秒 30758400 = 86400 * 356
-define(REFRESHTOKEN_VALID, 30758400).


-define (LOGIN_COLUMN, <<"id,account,mobile,password,email,
        nickname,avatar,gender,region,sign,status">>).

-define (DEF_USER_COLUMN, <<"id,account,
        nickname,avatar,sign,gender,region">>).
