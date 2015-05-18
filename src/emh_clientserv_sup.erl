-module(emh_clientserv_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Port =
        case application:get_env(port) of
            {ok, P} -> P;
            undefined -> 9090
        end,

    %% Set the socket into {active_once} mode.
    {ok, ListenSocket} = gen_tcp:listen(9023, [binary, {active,once}, {packet,line}]),
    spawn_link(fun empty_listeners/0),

    {ok, {{simple_one_for_one, 60, 3600},
    [{emh_clientserv,
     {emh_clientserv, start_link, [ListenSocket]},
     temporary, 1000, worker, [emh_clientserv]}
    ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
