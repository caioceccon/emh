-module(emh_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Port = case application:get_env(port) of
        {ok, P} -> P;
        undefined -> 9023
    end,

    {ok, {{one_for_one, 60, 3600},
    [
     {emh_hubservsup,
      {emh_hubserv_sup, start_link, []},
      permanent, 1000, supervisor, [emh_hubserv]},
     {emh_clientservsup,
      {emh_clientserv_sup, start_link, [Port]},
      permanent, 1000, supervisor, [emh_clientserv]},
     {emh_clientserv_udp,
      {emh_clientserv, start_link, [{udp, Port+1}]},
      permanent, 1000, worker, [emh_clientserv]}
    ]}}.
