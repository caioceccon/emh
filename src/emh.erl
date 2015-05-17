-module(emh).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
	emh_sup:start_link().

stop(_State) ->
	ok.