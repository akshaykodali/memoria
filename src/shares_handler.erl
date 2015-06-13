-module(shares_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{Method, Req2} = cowboy_req:method(Req),
	case Method of
		<<"GET">> ->
			{ok, Body} = memoria_share_dtl:render([]),
			{ok, Rep} = cowboy_req:reply(200, [], Body, Req2),
			{ok, Rep, State};
		<<"POST">> ->
			{ok, KeyValues, Req3} = cowboy_req:body_qs(Req),
			case parse_data(KeyValues) of
				{ok, ValidData} ->
					memoria_data:store(ValidData);
				{error, Errors} ->
					{ok, Body} = memoria_share_dtl:render([{errors, Errors}]),
					{ok, Rep} = cowboy_req:reply(200, [], Body, Req3),
					{ok, Rep, State}
			end
	end.

terminate(_Reason, _Req, _State) ->
	ok.

parse_data(KeyValues) ->
	Name = validate(name, KeyValues),
	ExpType = validate(exp_type, KeyValues),
	ExpTitle = validate(exp_title, KeyValues),
	Exp = validate(exp, KeyValues),
	Errors = lists:filter(fun(E) -> is_tuple(E) end, [Name, ExpType, ExpTitle, Exp]),
	case Errors of
		[] ->
			{ok, Name};
		_ ->
			{error, Errors}
	end.

validate(name, KeyValues) ->
	case lists:keyfind(<<"user_name">>, 1, KeyValues) of
		{_, Name} when Name /= <<"">> ->
			Name;
		_ ->
			{name, "is required"}
	end;
validate(exp_type, KeyValues) ->
	case lists:keyfind(<<"experience_type">>, 1, KeyValues) of
		{_, ExpType} when ExpType == <<"first_day_in_college">>;
			ExpType == <<"first_date">>;
			ExpType == <<"first_crush">>;
			ExpType == <<"first_movie_nightout">>;
			ExpType == <<"first_proposal">>;
			ExpType == <<"first_study_nighout">>;
			ExpType == <<"misc">>;
			ExpType == <<"meeting_bestfriend">> ->
				ExpType;
		_ -> {experience_type, "is required/invalid"}
	end;
validate(exp_title, KeyValues) ->
	case lists:keyfind(<<"experience_title">>, 1, KeyValues) of
		{_, ExpTitle} ->
			case byte_size(ExpTitle) < 10 of
				true -> {experience_title, "is short"};
				false -> ExpTitle
			end;
		_ ->
			{experience_title, "is required"}
	end;
validate(exp, KeyValues) ->
	case lists:keyfind(<<"experience">>, 1, KeyValues) of
		{_, Exp} -> 
			case byte_size(Exp) < 50 of
				true -> {experience, "is short"};
				false -> Exp
			end;
		undefined ->
			{experience, "is required"}
	end.
	