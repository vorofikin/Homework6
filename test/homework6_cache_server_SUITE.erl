-module(homework6_cache_server_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
  {ok, Pid} = homework6_cache_server:start(),
  [{pid, Pid}, {table_name, test_table} | Config].

end_per_suite(Config) ->
  gen_server:call(?config(pid, Config), stop),
  Config.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

all() -> [
  start_link_test,
  create_test,
  insert_test,
  insert_with_ttl_test,
  lookup_test,
  delete_test,
  lookup_undef_test
].

start_link_test(Config) ->
  Pid = ?config(pid, Config),
  ?assertEqual(true, is_pid(Pid)).

create_test(Config) ->
  TableName = gen_server:call(?config(pid, Config), {create, ?config(table_name, Config)}),
  ?assertEqual(test_table, TableName).

insert_test(Config) ->
  {ok, Key, Value} = gen_server:call(?config(pid, Config), {insert, ?config(table_name, Config), key, value}),
  ?assertEqual(key, Key),
  ?assertEqual(value, Value).

insert_with_ttl_test(Config) ->
  {ok, Key, Value} = gen_server:call(?config(pid, Config), {insert, ?config(table_name, Config), key3, value3, 5}),
  ?assertEqual(key3, Key),
  ?assertEqual(value3, Value),
  timer:sleep(7),
  InsertedValue = gen_server:call(?config(pid, Config), {lookup, ?config(table_name, Config), Key}),
  ?assertEqual(undefined, InsertedValue).

lookup_test(Config) ->
  Pid = ?config(pid, Config),
  Table = ?config(table_name, Config),
  {ok, Key, Value} = gen_server:call(Pid, {insert, Table, key1, value1}),
  {InsertedKey, InsertedValue} = gen_server:call(Pid, {lookup, Table, Key}),
  ?assertEqual(Key, InsertedKey),
  ?assertEqual(Value, InsertedValue).

lookup_undef_test(Config) ->
  Pid = ?config(pid, Config),
  Table = ?config(table_name, Config),
  undefined = gen_server:call(Pid, {lookup, Table, key2}).

delete_test(Config) ->
  Pid = ?config(pid, Config),
  Table = ?config(table_name, Config),
  ok = gen_server:call(Pid, {delete, Table, key1}),
  undefined = gen_server:call(Pid, {lookup, Table, key1}).
