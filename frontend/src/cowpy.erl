-module(cowpy).
-author('cooldaemon@gmail.com').

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
  application:start(cowboy),
  application:start(erlzmq),
  application:start(cowpy).

start(_Type, _Args) ->
  {ok, MP} = re:compile("-"),
  {ok, Context} = erlzmq:context(),
  Dispatch = [
    {'_', [
      {'_', cowpy_wsgi_handler, [MP, Context]}
    ]}
  ],
  {ok, _Pid} = cowboy:start_listener(cowpy_listener, 5,
    cowboy_tcp_transport, [{port, 8080}],
    cowboy_http_protocol, [{dispatch, Dispatch}]
  ),
  cowpy_sup:start_link(Context).

stop(_State) ->
  ok.
