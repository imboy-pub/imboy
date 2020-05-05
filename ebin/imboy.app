{application, 'imboy', [
	{description, "即时聊天软件"},
	{vsn, "0.1.0"},
	{modules, ['api_init_aas','auth_middleware','chat_handler','config_as','config_repo','friend_as','friend_category_ds','friend_category_repo','friend_handler','friend_repo','imboy_app','imboy_cipher','imboy_db','imboy_func','imboy_sup','init_handler','login_success_aas','myfriend_aas','passport_handler','resp_json_dto','route_helper','token_ds','user_as','user_ds','user_repo','websocket_as','websocket_handler','websocket_store_repo']},
	{registered, [imboy_sup]},
	{applications, [kernel,stdlib,ssl,mnesia,sync,lager,poolboy,mysql,jsx,cowboy]},
	{mod, {imboy_app, []}},
	{env, []}
]}.