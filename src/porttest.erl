-module(porttest).

-compile(export_all).

-define(TBL_NAME, the_table).
-define(CNT_NAME, the_table_cnt).

test_suite_new(PortNum, BlockSize) ->
    application:start(porttest),
    
    Block = <<0:(BlockSize * 8)>>,
    {PortInfo, Flusher} = test_port_new(Block),
    Ports = lists:duplicate(PortNum, PortInfo),

    ?TBL_NAME = ets:new(?TBL_NAME, [public, named_table, bag]),
    ?CNT_NAME = ets:new(?CNT_NAME, [public, named_table, set]),
    true = ets:insert(?CNT_NAME, [{good, 0}, {busy, 0}, {all, 0}]),
    {ok, Ports, Flusher}.

run_test_suite(Ports, Flusher, Count) ->
    io:format("*** test started ***~n"),
    put(test_suite_start, os:timestamp()),
    run_test_suite(Ports, Flusher, Count, 0).

run_test_suite(_, Flusher, Count, Count) ->
    io:format("*** test end ***~n"),
    ok = stop_flusher(Flusher),
    SuiteStartTime = get(test_suite_start),
    SuiteEndTime = os:timestamp(),
    Statistics = [element(2, X) || X <- ets:lookup(?TBL_NAME, tc)],
    All  = ets:lookup_element(?CNT_NAME, all, 2),
    Good = ets:lookup_element(?CNT_NAME, good, 2),
    Busy = ets:lookup_element(?CNT_NAME, busy, 2),
    TestTime = timer:now_diff(SuiteEndTime, SuiteStartTime),

    io:format("~p~n", [bear:get_statistics(Statistics)]),
    io:format("all loops count: ~w~ngood loops: ~w~nbusy: ~w~n",
              [All, Good, Busy]),
    io:format("test time: ~.3f seconds~n", [TestTime / 1000000]),
    io:format("avg RPS: ~.3f~n", [All / (TestTime / 1000000)]),

    ok;
run_test_suite(Ports, Flusher, Count, Current) ->
    Keys = [{element(2, Port),
             rpc:async_call(node(), ?MODULE, test_once, [Port])} || Port <- Ports],
    lists:foreach(
      fun({XPort, XKey}) ->
	      case rpc:nb_yield(XKey, 1000) of
                  {value, ok} ->
                      ok;
                  timeout ->
                      io:format("timeout:~n"),
                      Bad = what_wrong_with_this([XKey, Flusher], [XPort]),
                      io:format("~p~n", [Bad]),
                      exit(bad)
              end
      end,
      Keys),
    run_test_suite(Ports, Flusher, Count, Current + 1).

stop_flusher(Flusher) ->
    Flusher ! {stop, self()},
    receive
        {ok, Flusher} ->
            ok
    after 3000 ->
            exit('bad_flusher_can\'t stop')
    end.

test_once({Block, Port, SleepTime}) ->
    case erlang:port_command(Port, make_packet(Block), [nosuspend]) of
        true ->
            ets:update_counter(?CNT_NAME, good, 1);
        false ->
            ets:update_counter(?CNT_NAME, busy, 1),
            true = erlang:port_command(Port, make_packet(Block))
    end,
    ets:update_counter(?CNT_NAME, all, 1),
    timer:sleep(SleepTime),
    ok.

make_packet(Block) ->
    erlang:term_to_binary({os:timestamp(), Block}).

test_port_new(Block) ->
    Self = self(),
    Flusher =
        spawn_link(
          fun() ->
                  {ok, SleepTime} = application:get_env(porttest, sleep_time),
                  {ok, AfterSendSleep} = application:get_env(porttest, after_send_sleep),
                  Port = erlang:open_port({spawn, "./cport " ++ integer_to_list(SleepTime)},
                                          [use_stdio, binary, {packet, 4}]),
                  Self ! {port_info, {Block, Port, AfterSendSleep}},
                  test_port_flusher(Port)
          end),
    receive
        {port_info, Port} ->
            {Port, Flusher}
    end.

test_port_flusher(Port) ->
    receive
        {Port, {data, Block}} ->
            {StartTime, _} = binary_to_term(Block),
            true = ets:insert(?TBL_NAME, {tc, timer:now_diff(os:timestamp(), StartTime)}),
            test_port_flusher(Port);
        {stop, Pid} ->
            Pid ! {ok, self()};
        X ->
            exit({port_loop_badarg, X})
    end.
              
what_wrong_with_this(Pids, Ports) ->
    {what_wrong_with_pids(Pids), what_wrong_with_ports(Ports)}.

what_wrong_with_pids(Pids) ->
    [{Pid, (catch erlang:process_info(Pid))} || Pid <- Pids].

what_wrong_with_ports(Ports) ->
    [{Port, (catch erlang:port_info(Port))} || Port <- Ports].

