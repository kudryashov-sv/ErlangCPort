-module(loadport).

-compile(export_all).

-define(PROFILER_LOG, loadport_log).

run(Size, CheckLimit, ProcCount) ->
    Self = self(),
    spawn_link(
      fun() ->
              SizeBits = Size * 8,
              Block = <<0:SizeBits>>,

              Port = open_port(),
              Self ! {port, Port},

              lists:foreach(
                fun(_) ->
                        erlang:spawn_link(?MODULE, run_loop, [Port, Block, CheckLimit, 0])
                end,
                lists:seq(1, ProcCount)),
              endless_flush(Port)
      end),
    GoodPort = open_port(),
    receive
        {port, Port} ->
            {Port, open_port(), run_port_profiler({Port, GoodPort})}
    end.

run_port_profiler(Ports) ->
    disk_log:close(?PROFILER_LOG),
    {ok, ?PROFILER_LOG} =
        disk_log:open(
          [
           {file, "/tmp/test2.log"},
           {format, external},
           {name, ?PROFILER_LOG}
          ]
         ),
    
    Profiler = spawn_link(?MODULE, profiler_loop, [?PROFILER_LOG, Ports]),
    erlang:system_profile(Profiler, [runnable_ports]).

profiler_loop(Log, Ports = {Bad, Good}) ->
    receive
        Msg ->
            case Msg of
                {profile, Bad, _, _, _} ->
                    ok = disk_log:blog(Log, io_lib:format("from bad port: ~p~n", [Msg]));
                {profile, Good, _, _, _} ->
                    ok = disk_log:blog(Log, io_lib:format("from good port: ~p~n", [Msg]));
                _ ->
                    ok
            end,
            profiler_loop(Log, Ports)
    end.

open_port() ->
    erlang:open_port({spawn, "./cport 100"}, [{packet, 4}, eof, exit_status, use_stdio, binary]).

run_loop(Port, Block, CheckLimit, BusyCnt) ->
    case erlang:port_command(Port, Block, [nosuspend]) of
        true ->
            run_loop(Port, Block, CheckLimit, 0);
        false ->
            if
                BusyCnt + 1 > CheckLimit ->
                    check_dead(Port, Block, CheckLimit);
                true ->
                    run_loop(Port, Block, CheckLimit, BusyCnt + 1)
            end
    end.

check_dead(Port, Block, CheckLimit) ->
    io:format("~p: check port dead~n", [self()]),
    timer:sleep(3000),
    case erlang:port_command(Port, Block, [nosuspend]) of
        true ->
            io:format("not dead~n"),
            run_loop(Port, Block, CheckLimit, 0);
        false ->
            io:format("port dead: ~p~n", [Port]),
            ok
    end.

endless_flush(Port) ->
    receive
        {Port, {data, _}} ->
            endless_flush(Port);
        {Port, SomethingWrong} ->
            erlang:error({someting_wrong, SomethingWrong})
    end.

