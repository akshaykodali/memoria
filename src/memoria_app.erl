-module(memoria_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([set_server_header/4]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", public_pages_handler, []}]}
	]),
	{ok, _} = cowboy:start_http(memoria_app_listener, 30, [{port, 8000}],
		[
			{env, [{dispatch, Dispatch}]},
		 	{onresponse, fun ?MODULE:set_server_header/4}
		]
	),
	memoria_sup:start_link().

stop(_State) ->
	ok.

set_server_header(Status, Headers, Body, Rep) ->
	NewHeaders = lists:keyreplace(<<"server">>, 1, Headers, {<<"server">>, <<"afe">>}),
	{ok, NewRep} = cowboy_req:reply(Status, NewHeaders, Body, Rep),
	NewRep.