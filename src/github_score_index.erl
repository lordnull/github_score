%%% @doc A basic 'I'm alive" handler for cowboy.
-module(github_score_index).

-export([init/2]).

init(Req, State) ->
	Req1 = cowboy_req:reply(200,
		#{ <<"content-type">> => <<"text/plain">>},
		<<"Server is running.">>,
		Req),
	{ok, Req1, State}.
