-module(memoria_data).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([store/1, store/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {}).
-record(experience, {hash, name, exp_type, exp_title, exp}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	mnesia:start(),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Data) ->
	gen_server:call(?MODULE, {store, Data}).

store(Data, From) ->
	{Name, ExpType, ExpTitle, Exp} = Data,
	Hash = crypto:hash(md5, [Name, ExpType, ExpTitle, Exp]),
	Fun = fun() ->
			New = #experience{ hash = Hash, name = Name, exp_type = ExpType, exp_title = ExpTitle, exp = Exp },
			mnesia:write(New)
		  end,
	case mnesia:transaction(Fun) of
		{atomic, _} ->
			gen_server:reply(From, {ok, Hash});
		{aborted, Reason} ->
			gen_server:reply(From, {error, Reason})
	end.

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({store, Data}, From, State) ->
	spawn(memoria_data, store, [Data, From]),
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	error_logger:info_logger("~p received: ~p", [?MODULE, Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.