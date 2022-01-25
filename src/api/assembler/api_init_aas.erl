-module (api_init_aas).
%%%
% api_init_aas 是 api_init application assembler 缩写
%%%
-export ([data/0]).

%%%
%%%
data() ->
    [
        {<<"login_pwd_rsa_encrypt">>, config_logic:get("login_pwd_rsa_encrypt")}
        , {<<"login_rsa_pub_key">>, config_logic:get("login_rsa_pub_key")}
    ].
