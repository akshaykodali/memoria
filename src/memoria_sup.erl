-module(memoria_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
	Procs = [
		{
			memoria_validator,
			{memoria_validator, start_link, []},
			permanent,
			5000,
			worker,
			[memoria_validator]
		},
		{
			memoria_data,
			{memoria_data, start_link, []},
			permanent,
			5000,
			worker,
			[memoria_data]
		}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
