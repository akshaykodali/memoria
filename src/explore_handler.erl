-module(explore_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{Method, Req2} = cowboy_req:method(Req),
	handle(Method, Req2, State).

handle(<<"GET">>, Req, State) ->
	case cowboy_req:bindings(Req) of
		{[{hash, Hash}], Req2} ->
			case memoria_data:retrieve(Hash) of
				{ok, {experience, _, Name, ExpType, ExpTitle, Exp, _}} ->
					{ok, Body} = memoria_memoir_dtl:render([
															{name, Name},
															{exp_type, ExpType},
															{exp_title, ExpTitle},
															{exp, binary:split(Exp, <<"\n">>, [global])}
														   ]),
					{ok, Rep} = cowboy_req:reply(200, [], Body, Req2),
					{ok, Rep, State};
				_ ->
					{ok, Rep} = cowboy_req:reply(404, Req2),
					{ok, Rep, State}
			end;
		_ ->
			Date = erlang:date(),
			{ok, Experiences} = memoria_data:list_experiences(Date),
			{ok, Body} = memoria_explore_dtl:render([{experiences, Experiences}]),
			{ok, Rep} = cowboy_req:reply(200, [], Body, Req),
			{ok, Rep, State}
	end;
handle(<<"POST">>, Req, State) ->
	{ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
	case parse_data(date, KeyValues) of
		{ok, Date} ->
			{ok, Experiences} = memoria_data:list_experiences(Date),
			{ok, Body} = memoria_compact_explore_dtl:render([{experiences, Experiences}]),
			{ok, Rep} = cowboy_req:reply(200, [], Body, Req2),
			{ok, Rep, State}
	end.

parse_data(date, KeyValues) ->
	Date = validate(date, KeyValues),
	Error = lists:filter(fun(E) -> is_tuple(E) end, [Date]),
	case Error of
		[] ->
			ValidData = binary:split(Date, <<",">>, [global]),
			FormattedData = lists:map(fun(E) -> binary_to_integer(E) end, ValidData),
			Result = list_to_tuple(FormattedData),
			{ok, Result};
		_ ->
			{error, Error}
	end.

validate(date, KeyValues) ->
	case lists:keyfind(<<"date">>, 1, KeyValues) of
		{_, Date} when Date /= <<"">> ->
			Date;
		_ ->
			{date, "is required"}
	end.

terminate(_Reason, _Req, _State) ->
	ok.
