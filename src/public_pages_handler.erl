-module(public_pages_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{ok, Body} = memoria_home_dtl:render([]),
	{ok, Rep} = cowboy_req:reply(200, [], Body, Req),
	{ok, Rep, State}.

terminate(_Reason, _Req, _State) ->
	ok.
