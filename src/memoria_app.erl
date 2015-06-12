-module(memoria_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([set_server_header/4]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
				{"/", public_pages_handler, []},
				{"/assets/[...]", cowboy_static, {priv_dir, memoria, "assets"}}
			  ]}
	]),
	{ok, _} = cowboy:start_http(memoria_app_listener, 30, [{port, 8000}],
		[
			{env, [{dispatch, Dispatch}]}
		 	
		]
	),
	memoria_sup:start_link().

stop(_State) ->
	ok.