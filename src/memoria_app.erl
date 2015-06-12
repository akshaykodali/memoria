-module(memoria_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
				{"/", public_pages_handler, []},
				{"/share", shares_handler, []},
				{"/assets/[...]", cowboy_static, {priv_dir, memoria, "assets"}}
			  ]}
	]),
	{ok, _} = cowboy:start_http(memoria_app_listener, 30, [{port, 8000}],
		[
			{env, [{dispatch, Dispatch}]}
		 	
		]
	),
	memoria_sup:start_link(Args).

stop(_State) ->
	ok.