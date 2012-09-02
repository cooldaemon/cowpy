-module(cowpy_broker).
-author('cooldaemon@gmail.com').

-export([start_link/1]).
-export([init/2]).

start_link(Context) ->
  proc_lib:start_link(?MODULE, init, [self(), Context]).

init(Parent, Context) ->
  {Frontend, Backend} = bind(Context),
  proc_lib:init_ack(Parent, {ok, self()}),
  queueing(Frontend, Backend),
  close(Frontend, Backend).

bind(Context) ->
  {ok, Frontend} = erlzmq:socket(Context, [router, {active, true}]),
  ok = erlzmq:bind(Frontend, "inproc://broker"),

  {ok, Backend} = erlzmq:socket(Context, [dealer, {active, true}]),
  ok = erlzmq:bind(Backend, "tcp://*:5560"),

  {Frontend, Backend}.

queueing(Frontend, Backend) ->
  receive
    {zmq, Frontend, Msg, Flags} ->
      case proplists:get_bool(rcvmore, Flags) of
        true ->
          erlzmq:send(Backend, Msg, [sndmore]);
        false ->
          erlzmq:send(Backend, Msg)
      end;
    {zmq, Backend, Msg, Flags} ->
      case proplists:get_bool(rcvmore, Flags) of
        true ->
          erlzmq:send(Frontend, Msg, [sndmore]);
        false ->
          erlzmq:send(Frontend, Msg)
      end
  end,
  queueing(Frontend, Backend).

close(Frontend, Backend) ->
  erlzmq:close(Frontend),
  erlzmq:close(Backend).
