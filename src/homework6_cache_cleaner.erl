-module(homework6_cache_cleaner).
%%
%%-behavior(gen_server).
%%
%%-export([start_link/1, init/1, handle_info/2, code_change/3, terminate/2]).
%%-export([init/1, delete/2]).
%%-record(state, {cache_server, cleanup_timer}).
%%
%%start_link(ServerPid) ->
%%  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%
%%init([]) ->
%%  TimerRef = erlang:send_after(60000, self(), cleanup),
%%  {ok, TimerRef}.
%%
%%delete(TableName, Key) ->
%%  receive
%%    {}
%%  end
%%  .
%%
%%handle_info(delete_value, TableName) ->
%%  CurrentTime = erlang:system_time(second),
%%  Records = ets:tab2list(TableName),
%%  NewRecords = lists:foldl(fun({Key, Value, Unixtime}, Acc) ->
%%    case Unixtime > CurrentTime of
%%      true -> ets:delete(TableName, Key),
%%        Acc;
%%      false -> [{Key, Value, Unixtime} | Acc]
%%    end
%%  end, [], Records),
%%  {noreply, NewRecords}.
%%
%%handle_info(cleanup, State) ->
%%  ServerPid = State#state.cache_server,
%%  ServerPid ! {delete_obsolete, self()},
%%  NewTimerRef = erlang:send_after(60000, self()),
%%  {ok, State#state{cleanup_timer = NewTimerRef}}.
%%
%%terminate(_Reason, _State) ->
%%  ok.
%%
%%code_change(_OldVersion, State, _Extra) ->
%%  {ok, State}.
