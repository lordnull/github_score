-module(github_score_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_',
			[ {"/", github_score_index, {}}
			]
		}]),
	% I'm using 'start clear' rather than https because experience has told me
	% it's easier to just put this behind a reverse proxy (link nginx) and dev
	% in plain http than it is to support https directly.
	{ok, _} = cowboy:start_clear(github_score_listener,
		% port arbitrarity chosen for dev, later this will be configurable
		[ {port, 7654} ]
		, #{ env => #{ dispatch => Dispatch}}
		),

	github_score_sup:start_link().

stop(_State) ->
	ok.
