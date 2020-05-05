{application, 'imboy', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['handler_hello','imboy_app','imboy_sup']},
	{registered, [imboy_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {imboy_app, []}},
	{env, []}
]}.