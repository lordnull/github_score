%%% @doc the score handler endpoint for the rest api.
-module(github_score_handler).

-export([init/2]).

init(Req, #{ mode := a_user} = State) ->
	% The route _must_ have a binding for 'user'.
	Reply = case cowboy_req:binding(user, Req) of
		undefined ->
			cowboy_req:reply(404,
				#{ <<"content-type">> => <<"text/plain">>},
				<<"Invalid user identifier">>, Req);
		User ->
			{ok, N} = github_score_datastore:get_score(User),
			cowboy_req:reply(200,
				#{ <<"content-type">> => <<"test/plain">>},
				io_lib:format("~p", [N]), Req)
	end,
	{ok, Reply, State}.
