-module(homework6).

-behavior(gen_server).

-export([start_link/0, create/1, insert/3, insert/4, lookup/2, delete/2, stop/0, start/0]).

-export([init/1, handle_call/3, terminate/2, code_change/3, handle_info/2, handle_cast/2]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_server:start(?MODULE, [], []).

create(TableName) ->
  gen_server:call(?MODULE, {create, TableName}).

insert(TableName, Key, Value) ->
  gen_server:call(?MODULE, {insert, TableName, Key, Value}).

insert(TableName, Key, Value, Ttl) ->
  gen_server:call(?MODULE, {insert, TableName, Key, Value, Ttl}).

lookup(TableName, Key) ->
  gen_server:call(?MODULE, {lookup, TableName, Key}).

delete(TableName, Key) ->
  gen_server:call(?MODULE, {delete, TableName, Key}).

stop() ->
  gen_server:stop(?MODULE).

init([]) ->
  {ok, []}.

handle_call({create, TableName}, _From, State) ->
  Table = ets:new(TableName, [public, ordered_set, named_table]),
  erlang:send_after(60000, self(), {delete_obsolete, Table}),
  {reply, Table, State};
handle_call({insert, TableName, Key, Value, Ttl}, _From, State) ->
  ExpireTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()) + Ttl,
  ets:insert(TableName, {Key, Value, ExpireTime}),
  {reply, {ok, Key, Value}, State};
handle_call({insert, TableName, Key, Value}, _From, State) ->
  ets:insert(TableName, {Key, Value}),
  {reply, {ok, Key, Value}, State};
handle_call({lookup, TableName, Key}, _From, State) ->
  CurrentUnixTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  case ets:lookup(TableName, Key) of
    {Key, Value} -> {reply, Value, State};
    [{Key, Value}] -> {reply, Value, State};
    [{Key, Value, ExpireTime}] when CurrentUnixTime =< ExpireTime ->
      {reply, Value, State};
    [{Key, _Value, _ExpireTime}] ->
      ets:delete(TableName, Key),
      {reply, undefined, State};
    _ -> {reply, undefined, State}
  end;
handle_call({delete, TableName, Key}, _From, State) ->
  ets:delete(TableName, Key),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({delete_obsolete, TableName}, State) ->
  io:format(delete),
  delete_obsolete(TableName),
  erlang:send_after(60000, self(), {delete_obsolete, TableName}),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  normal.

delete_obsolete(TableName) ->
  CurrentUnixtime = get_unixtime(),
  FirstKey = ets:first(TableName),
  ok = delete_obsolete(TableName, FirstKey, CurrentUnixtime).

delete_obsolete(_TableName, '$end_of_table', _Unixtime) ->
  ok;
delete_obsolete(TableName, Key, Unixtime) ->
  case ets:lookup(TableName, Key) of
    [{Key, _Value, EndDateSec}] when Unixtime >= EndDateSec ->
      ets:delete(TableName, Key);
    _ -> true
  end,
  delete_obsolete(TableName, ets:next(TableName, Key), Unixtime).

get_unixtime() ->
  calendar:datetime_to_gregorian_seconds(calendar:local_time()).
