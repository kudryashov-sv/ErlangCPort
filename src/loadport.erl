-module(loadport).

-export([
         main/1,
         run/3,
         run_loop/5
        ]).

main([PacketSize, CheckLimit, ProcCount]) ->
    PkSize  = list_to_integer(PacketSize),
    ChLimit = list_to_integer(CheckLimit),
    PcCount = list_to_integer(ProcCount),

    run(PkSize, ChLimit, PcCount).

run(Size, CheckLimit, ProcCount) ->
    Self = self(),
    spawn_link(
      fun() ->
              SizeBits = Size * 8,
              Block = <<0:SizeBits>>,

              Port = open_port(),
              
              Testers =
                  lists:map(
                    fun(_) ->
                            erlang:spawn_link(?MODULE, run_loop, [Self, Port, Block, CheckLimit, 0])
                    end,
                    lists:seq(1, ProcCount)),
              Self ! {test_info, Port, Testers},
              endless_flush(Port)
      end),
    receive
        {test_info, Port, Testers} ->
            io:format("wait testers~n"),
            ok = wait_testers(Testers),
            io:format("*** Port: ~p - dead ***~n", [Port]),
            Port
    end.

wait_testers(Testers) ->
    lists:foreach(
      fun(Pid) ->
              receive
                  {Pid, port_dead} ->
                      ok
              end
      end, Testers).

open_port() ->
    erlang:open_port({spawn, "./cport 0"}, [{packet, 4}, eof, exit_status, use_stdio, binary]).

run_loop(RootProc, Port, Block, CheckLimit, BusyCnt) ->
    case erlang:port_command(Port, Block, [nosuspend]) of
        true ->
            run_loop(RootProc, Port, Block, CheckLimit, 0);
        false ->
            if
                BusyCnt + 1 > CheckLimit ->
                    check_dead(RootProc, Port, Block, CheckLimit);
                true ->
                    run_loop(RootProc, Port, Block, CheckLimit, BusyCnt + 1)
            end
    end.

check_dead(RootProc, Port, Block, CheckLimit) ->
    io:format("~p: check port dead~n", [self()]),
    timer:sleep(5000),
    case erlang:port_command(Port, Block, [nosuspend]) of
        true ->
            io:format("not dead~n"),
            run_loop(RootProc, Port, Block, CheckLimit, 0);
        false ->
            io:format("port dead: ~p~n", [Port]),
            RootProc ! {self(), port_dead},
            ok
    end.

endless_flush(Port) ->
    receive
        {Port, {data, _}} ->
            endless_flush(Port);
        {Port, SomethingWrong} ->
            erlang:error({someting_wrong, SomethingWrong})
    end.

