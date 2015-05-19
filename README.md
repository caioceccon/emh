# EMH - Erlang Message Hub

## Compilation
    git clone git@github.com:caioceccon/emh.git
    cd emh
    erl -make

## Running
    # Erlang required
    erl -pa ebin/
    application:start(emh).

## Using TCP version with telnet
    telnet localhost 9090
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    Welcome to Erlang Message Hub!!
    To add a Hub use `Add:HubName`
    To remove a Hub use `Remove:HubName`
    To list Hubs use `List:`
    To subscribe a Hub use `Subscribe:HubName`
    To unsubscribe a Hub use `Unsubscribe:HubName`
    To publish a Hub use `Publish:HubName:Message`

## Using UDP
UDP support the same protocol that TCP except for the Subscribe and Unsubscribe commands not yet implemented.
