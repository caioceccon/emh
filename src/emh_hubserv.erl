% Message Hub
% Develop an in-memory message hub. The hub must support publish and subscribe actions on 
% arbitrary data sets via TCP and UDP.

-module(emh_hubserv).
-behaviour(gen_server).

-compile([debug_info]).

-export([start_link/0, add_hub/1, remove_hub/1, list_hubs/0,
         add_client/2, remove_client/2, publish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(hub, {clients=orddict:new()}).

%% Client API.
start_link() -> gen_server:start_link({local, hubserv}, ?MODULE, [], []).

add_hub(Name) ->
    gen_server:call(hubserv, {add, Name}).

remove_hub(Name) ->
    gen_server:call(hubserv, {remove, Name}).

list_hubs() ->
    gen_server:call(hubserv, list).

add_client(ClientPid, HubName) ->
    gen_server:call(hubserv, {subscribe, ClientPid, HubName}).

remove_client(ClientPid, HubName) ->
    gen_server:call(hubserv, {unsubscribe, ClientPid, HubName}).

publish(HubName, Msg) ->
    gen_server:cast(hubserv, {publish, HubName, Msg}).

%%% Server functions
init([]) -> {ok, orddict:new()}.

%% Add a hub.
handle_call({add, Name}, _From, Hubs) when is_bitstring(Name) ->
    {reply, ok, make_hub(Name, Hubs)};

%% Remove a hub.
handle_call({remove, Name}, _From, Hubs) when is_bitstring(Name) ->
    {reply, ok, orddict:erase(Name, Hubs)};

%% List hubs.
handle_call(list, _From, Hubs) ->
    {reply, orddict:fetch_keys(Hubs), Hubs};

%% Subscribe hub.
handle_call({subscribe, ClientPid, HubName}, _From, Hubs) when is_bitstring(HubName),
                                                               is_pid(ClientPid) ->
    {reply, ok, subscribe(ClientPid, HubName, Hubs)};

%% Unsubscribe hub.
handle_call({unsubscribe, ClientPid, HubName}, _From, Hubs) when is_bitstring(HubName),
                                                                 is_pid(ClientPid) ->
    {reply, ok, unsubscribe(ClientPid, HubName, Hubs)}.

%% Publish hub.
handle_cast({publish, HubName, Msg}, Hubs) when is_bitstring(HubName),
                                                is_bitstring(Msg) ->
    publish_msg(HubName, Msg, Hubs),
    {noreply, Hubs}.

handle_info(Msg, Hubs) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, Hubs}.

terminate(normal, Hubs) ->
    [io:format("Hub ~p will be closed.~n", [H]) || H <- orddict:fetch_keys(Hubs)],
    ok.

code_change(_OldVsn, Hubs, _Extra) ->
    {ok, Hubs}.    

%%% Private functions
make_hub(Name, Hubs) ->
    case orddict:is_key(Name, Hubs) of
        true ->
            Hubs;
        false ->
            orddict:store(Name, #hub{}, Hubs)
    end.

subscribe(ClientPid, HubName, Hubs) ->
    case orddict:find(HubName, Hubs) of
        {ok, H} ->
            NewClients = orddict:store(ClientPid, true, H#hub.clients),
            orddict:store(HubName, #hub{clients=NewClients}, Hubs);
        error -> erlang:error(hub_not_found)
    end.

unsubscribe(ClientPid, HubName, Hubs) ->
    case orddict:find(HubName, Hubs) of
        {ok, H} ->
            NewClients = orddict:erase(ClientPid, H#hub.clients),
            orddict:store(HubName, #hub{clients=NewClients}, Hubs);
        error -> erlang:error(hub_not_found)
    end.

publish_msg(HubName, Msg, Hubs) ->
    case orddict:find(HubName, Hubs) of
        {ok, _Hub = #hub{clients=Clients}} ->
            [C ! {msg, Msg} || C <- orddict:fetch_keys(Clients)];
        error -> erlang:error(hub_not_found)
    end.





