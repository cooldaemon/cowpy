-module(cowpy_sup).
-author('cooldaemon@gmail.com').

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Context) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Context]).

init([Context]) ->
  {ok, {
    {one_for_one, 10, 10},
    [
      {
        cowpy_broker,
        {cowpy_broker, start_link, [Context]},
        permanent,
        2000,
        worker,
        []
      }
    ]
  }}.
