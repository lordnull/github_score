-module(github_score_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	github_score_sup:start_link().

stop(_State) ->
	ok.
