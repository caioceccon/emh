-module(emh_hubserv_sup).
-behaviour(supervisor).
 
-export([start_link/0, init/1]).
 
start_link() ->
supervisor:start_link({local, ?MODULE}, ?MODULE, []).
 
init([]) ->
    {ok, {{one_for_one, 60, 3600},
    [{emh_hubserv,
      {emh_hubserv, start_link, []},
      permanent, 1000, worker, [emh_hubserv]}
    ]}}.