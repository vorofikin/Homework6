-module(homework6_cache_server).

-behavior(gen_server).

-export([start_link/0, create/1, insert/3, insert/4, lookup/2, delete/2, stop/0, start/0]).

-export([init/1, handle_call/3, terminate/2, code_change/3, handle_info/2, handle_cast/2]).

-record(state, {cache_worker}).

-define(SERVER, ?MODULE).

start_link() ->
  #state{},
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
%%  WorkerPid = start_worker(),
%%  #state{cache_worker = WorkerPid},
  {ok, []}.

handle_call({create, TableName}, _From, State) ->
  Table = ets:new(TableName, [public, set, named_table]),
  {reply, Table, State};
handle_call({insert, TableName, Key, Value, Ttl}, _From, State) ->
  ExpireTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()) + Ttl,
  ets:insert(TableName, {Key, Value, ExpireTime}),
%%  erlang:send_after(Ttl, #state.cache_worker, {TableName, Key}),
  {reply, {ok, Key, Value}, State};
handle_call({insert, TableName, Key, Value}, _From, State) ->
  ets:insert(TableName, {Key, Value}),
  {reply, {ok, Key, Value}, State};
handle_call({lookup, TableName, Key}, _From, State) ->
  CurrentUnixTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  case ets:lookup(TableName, Key) of
    {Key, Value} -> {reply, {Key, Value}, State};
    [{Key, Value}] -> {reply, {Key, Value}, State};
    {Key, Value, ExpireTime} when CurrentUnixTime =< ExpireTime ->
      {reply, {Key, Value}, State};
    {Key, Value, ExpireTime} ->
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

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  normal.

start_worker() ->
  WorkerPid = spawn_link(fun() ->
    receive
      {TableName, Key} ->
        ets:delete(TableName, Key)
    end,
    check_values()
    end
  ),
  io:format("worker started: ~p~n", [WorkerPid]),
  WorkerPid.

%%stop_worker() ->
%%  stop

check_values() ->
%%  CurrentTime = erlang:system_time(second),
  receive
    {TableName, Key} ->
      ets:delete(TableName, Key)
  end,
  check_values().
