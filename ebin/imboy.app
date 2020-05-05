{application, 'imboy', [
	{description, "即时聊天软件"},
	{vsn, "0.1.0"},
	{modules, ['api_init_aas','auth_middleware','config_as','config_repo','dtl_handler','imboy_app','imboy_cipher','imboy_db','imboy_func','imboy_sup','init_handler','login_success_aas','passport_handler','resp_json_dto','route_helper','token_ds','user_as','user_repo','websocket_handler']},
	{registered, [imboy_sup]},
	{applications, [kernel,stdlib,ssl,sync,lager,poolboy,mysql,jsx,erlydtl,cowboy]},
	{mod, {imboy_app, []}},
	{env, []}
]}.