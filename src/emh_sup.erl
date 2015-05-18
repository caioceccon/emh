-module(emh_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 60, 3600},
    [
     {emh_hubservsup,
      {emh_hubserv_sup, start_link, []},
      permanent, 1000, supervisor, [emh_hubserv]},
     {emh_clientservsup,
      {emh_clientserv_sup, start_link, []},
      permanent, 1000, supervisor, [emh_clientserv]}
    ]}}.
