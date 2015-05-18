-module(emh_clientserv).

-behaviour(gen_server).

-compile([debug_info, export_all]).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER_HELLO, "Welcome to Erlang Message Hub!!\r\n"
                      "To add a Hub use `Add:HubName`\r\n"
                      "To remove a Hub use `Remove:HubName`\r\n"
                      "To subscribe a Hub use `Subscribe:HubName`\r\n"
                      "To unsubscribe a Hub use `Unsubscribe:HubName`\r\n"
                      "To publish a Hub use `Publish:HubName:Message`\r\n").

start_link(Socket) -> gen_server:start_link(?MODULE, Socket, []).

%%% Server functions
init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, Socket}.

handle_cast(accept, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    emh_clientserv_sup:start_socket(),
    send(AcceptSocket, ?SERVER_HELLO, []),
    {noreply, AcceptSocket}.

handle_info({tcp, _Socket, <<"Add:", Hub/binary>>}, Socket) ->
    send(Socket, add_hub(Hub), []),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"List:", _Rest/binary>>}, Socket) ->
    send(Socket, list_hubs(), []),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Remove:", Hub/binary>>}, Socket) ->
    send(Socket, remove_hub(Hub), []),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Subscribe:", Hub/binary>>}, Socket) ->
    send(Socket, subscribe_hub(Hub), []),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Unsubscribe:", Hub/binary>>}, Socket) ->
    send(Socket, unsubscribe_hub(Hub), []),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Publish:", HubMsg/binary>>}, Socket) ->
    send(Socket, publish_hub(HubMsg), []),
    {noreply, Socket};

handle_info({msg, Msg}, Socket) ->
    send(Socket, Msg, []),
    {noreply, Socket};

handle_info({tcp_closed, _Socket}, Socket) ->
    {stop, normal, Socket};

handle_info({tcp, _Socket, Str}, Socket) ->
    send(Socket, <<"Unknown command~p">>, [Str]),
    {noreply, Socket}.

terminate(normal, _Socket) ->
    io:format("Client ~p has disconnected.~n", [self()]),
    ok.

code_change(_OldVsn, Hubs, _Extra) ->
    {ok, Hubs}.

%% Private functions.
add_hub(HubName) ->
    emh_hubserv:add_hub(remove_blank_spaces(HubName)).

remove_hub(HubName) ->
    emh_hubserv:remove_hub(remove_blank_spaces(HubName)).

list_hubs() ->
    string:join([binary_to_list(B) || B <- emh_hubserv:list_hubs()], ", ").

subscribe_hub(HubName) ->
    emh_hubserv:add_client(self(), remove_blank_spaces(HubName)).

unsubscribe_hub(HubName) ->
    emh_hubserv:remove_client(self(), remove_blank_spaces(HubName)).

publish_hub(HubMsg) ->
    HubMsgStr = binary_to_list(HubMsg),
    ColonPosition = string:str(HubMsgStr, ":"),
    HubName = list_to_binary(string:substr(HubMsgStr, 1, ColonPosition - 1)),
    Msg = list_to_binary(string:substr(HubMsgStr, ColonPosition + 1)),
    emh_hubserv:publish(HubName, Msg).

send(Socket, <<BitStr/binary>>, Args) ->
    send(Socket, binary_to_list(BitStr), Args);

send(Socket, Atom, Args) when is_atom(Atom) ->
    send(Socket, atom_to_list(Atom), Args);

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

remove_blank_spaces(Str) ->
    re:replace(Str, "(\\s+\\s+)", "", [{return,binary}]).
