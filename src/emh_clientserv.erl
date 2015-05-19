-module(emh_clientserv).

-behaviour(gen_server).

-compile([debug_info, export_all]).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER_HELLO, "Welcome to Erlang Message Hub!!\r\n"
                      "To add a Hub use `Add:HubName`\r\n"
                      "To remove a Hub use `Remove:HubName`\r\n"
                      "To list Hubs use `List:`\r\n"
                      "To subscribe a Hub use `Subscribe:HubName`\r\n"
                      "To unsubscribe a Hub use `Unsubscribe:HubName`\r\n"
                      "To publish a Hub use `Publish:HubName:Message`\r\n").

%%% Server functions
start_link({tcp, Socket}) ->
    gen_server:start_link(?MODULE, {tcp, Socket}, []);
start_link({udp, Port}) ->
    gen_server:start_link(?MODULE, {udp, Port}, []).

init({tcp, Socket}) ->
    gen_server:cast(self(), accept),
    {ok, Socket};

init({udp, Port}) ->
    {ok, UdpSocket} = gen_udp:open(Port, [binary, {active,true}]),
    {ok, UdpSocket}.

handle_cast(accept, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    emh_clientserv_sup:start_socket(),
    send(tcp, AcceptSocket, ?SERVER_HELLO),
    {noreply, AcceptSocket}.

%% TCP Handlers.
handle_info({tcp, _Socket, <<"Add:", Hub/binary>>}, Socket) ->
    send(tcp, Socket, add_hub(Hub)),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"List:", _Rest/binary>>}, Socket) ->
    send(tcp, Socket, list_hubs()),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Remove:", Hub/binary>>}, Socket) ->
    send(tcp, Socket, remove_hub(Hub)),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Subscribe:", Hub/binary>>}, Socket) ->
    send(tcp, Socket, subscribe_hub(Hub)),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Unsubscribe:", Hub/binary>>}, Socket) ->
    send(tcp, Socket, unsubscribe_hub(Hub)),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Publish:", HubMsg/binary>>}, Socket) ->
    send(tcp, Socket, publish_hub(HubMsg)),
    {noreply, Socket};

handle_info({msg, Msg}, Socket) ->
    send(tcp, Socket, Msg),
    {noreply, Socket};

%%% UDP Handlers.
handle_info({udp, _UdpSocket, Address, Port, <<"Add:", HubName/binary>>}, Socket) ->
    send(udp, {Socket, Address, Port}, add_hub(HubName)),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"Remove:", HubName/binary>>}, Socket) ->
    send(udp, {Socket, Address, Port}, remove_hub(HubName)),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"List:", _Rest/binary>>}, Socket) ->
    send(udp, {Socket, Address, Port}, list_hubs()),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"Publish:", HubMsg/binary>>}, Socket) ->
    send(udp, {Socket, Address, Port}, publish_hub(HubMsg)),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"Subscribe:", _HubMsg/binary>>}, Socket) ->
    send(udp, {Socket, Address, Port}, <<"Subscribe not implemented for UDP">>),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"Unsubscribe:", _HubMsg/binary>>}, Socket) ->
    send(udp, {Socket, Address, Port}, <<"Unsubscribe not implemented for UDP.">>),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<Str/binary>>}, Socket) ->
    Msg = io_lib:format(<<"Unknown command~p">>, [Str]),
    send(udp, {Socket, Address, Port}, Msg),
    {noreply, Socket};

handle_info({tcp_closed, _Socket}, Socket) ->
    {stop, normal, Socket};

handle_info({tcp, _Socket, Str}, Socket) ->
    Msg = io_lib:format(<<"Unknown command~p">>, [Str]),
    send(tcp, Socket, Msg),
    {noreply, Socket}.

handle_call(_, _From, Socket) ->
    {reply, ok, Socket}.

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
    case string:str(HubMsgStr, ":") of
        ColonPosition when ColonPosition > 0 ->
            ColonPosition = string:str(HubMsgStr, ":"),
            HubName = list_to_binary(string:substr(HubMsgStr, 1, ColonPosition - 1)),
            Msg = list_to_binary(string:substr(HubMsgStr, ColonPosition + 1)),
            emh_hubserv:publish(HubName, Msg);
        ColonPosition when  ColonPosition =:= 0 -> unknown_pattern
    end.

send(Protocol, Socket, <<BitStr/binary>>) ->
    send(Protocol, Socket, binary_to_list(BitStr));

send(Protocol, Socket, Atom) when is_atom(Atom) ->
    send(Protocol, Socket, atom_to_list(Atom));

send(tcp, Socket, Str) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", [])),
    ok = inet:setopts(Socket, [{active, once}]),
    ok;

send(udp, {Socket, Address, Port}, Str) ->
  gen_udp:send(Socket, Address, Port, Str).

remove_blank_spaces(Str) ->
    re:replace(Str, "(\\s+\\s+)", "", [{return,binary}]).
