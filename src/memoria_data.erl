-module(memoria_data).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([store/1, store/2, retrieve/1, retrieve/2]).
-export([list_experiences/1, list_experiences/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include_lib("stdlib/include/qlc.hrl").

-record(state, {}).
-record(experience, {hash, name, exp_type, exp_title, exp, created_on}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	mnesia:start(),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Data) ->
	gen_server:call(?MODULE, {store, Data}).

store(Data, From) ->
	{Name, ExpType, ExpTitle, Exp} = Data,
	Hash = md5_hash([Name, ExpType, ExpTitle, Exp]),
	Date = erlang:date(),
	Fun = fun() ->
			New = #experience{
								hash = Hash,
								name = Name,
								exp_type = ExpType,
								exp_title = ExpTitle,
								exp = Exp,
								created_on = Date
							 },
			mnesia:write(New)
		  end,
	case mnesia:transaction(Fun) of
		{atomic, _} ->
			gen_server:reply(From, {ok, Hash});
		{aborted, Reason} ->
			gen_server:reply(From, {error, Reason})
	end.

retrieve(Hash) ->
	gen_server:call(?MODULE, {retrieve, Hash}).

retrieve(Hash, From) ->
	F = fun() ->
			Q = qlc:q([E || E <- mnesia:table(experience), E#experience.hash == Hash]),
			qlc:e(Q)
		end,
	case mnesia:transaction(F) of
		{atomic, [ExpRecord]} ->
			gen_server:reply(From, {ok, ExpRecord});
		_ ->
			gen_server:reply(From, {error, not_found})
	end.

list_experiences(Date) ->
	gen_server:call(?MODULE, {list_exp, Date}).

list_experiences(Date, From) ->
	F = fun() ->
			Q = qlc:q([E || E <- mnesia:table(experience), E#experience.created_on == Date]),
			qlc:e(Q)
		end,
	case mnesia:transaction(F) of
		{atomic, ExpRecords} ->
			gen_server:reply(From, {ok, ExpRecords});
		_ ->
			gen_server:reply(From, {error, not_found})
	end.

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({store, Data}, From, State) ->
	spawn(memoria_data, store, [Data, From]),
	{noreply, State};
handle_call({retrieve, Hash}, From, State) ->
	spawn(memoria_data, retrieve, [Hash, From]),
	{noreply, State};
handle_call({list_exp, Date}, From, State) ->
	spawn(memoria_data, list_experiences, [Date, From]),
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

%% Private.
md5_hash(Data) ->
	BinHash = crypto:hash(md5, Data),
	BinList = binary_to_list(BinHash),
	DeepList = lists:map(fun(X) -> int_to_hex(X) end, BinList),
	list_to_binary(lists:flatten(DeepList)).

int_to_hex(Num) when Num < 256 ->
	[hex(Num div 16), hex(Num rem 16)].

hex(Num) when Num < 10 ->
	$0 + Num;
hex(Num) when Num >= 10, Num < 16 ->
	$a + (Num - 10).
