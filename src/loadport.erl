-module(loadport).

-compile(export_all).

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
    receive
        {port, Port} ->
            {Port, open_port()}
    end.

open_port() ->
    erlang:open_port({spawn, "/bin/sh -c cat"}, [eof, exit_status, use_stdio, binary]).

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

