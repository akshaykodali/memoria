-module(explore_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	case cowboy_req:bindings(Req) of
		{[{hash, Hash}], Req2} ->
			case memoria_data:retrieve(Hash) of
				{ok, {experience, _, Name, ExpType, ExpTitle, Exp}} ->
					{ok, Body} = memoria_memoir_dtl:render([
															{name, Name},
															{exp_type, ExpType},
															{exp_title, ExpTitle},
															{exp, Exp}
														   ]),
					{ok, Rep} = cowboy_req:reply(200, [], Body, Req2),
					{ok, Rep, State};
				_ ->
					{ok, Rep} = cowboy_req:reply(404, Req2),
					{ok, Rep, State}
			end;
		_ ->
			{ok, Body} = memoria_explore_dtl:render([]),
			{ok, Rep} = cowboy_req:reply(200, [], Body, Req),
			{ok, Rep, State}
	end.

terminate(_Reason, _Req, _State) ->
	ok.
