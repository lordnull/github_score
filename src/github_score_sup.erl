-module(github_score_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs =
	[ #{ id => github_score_datastore ,
		 start => {github_score_datastore, start_link, []} }
	, #{ id => github_score_poller,
		 start => {github_score_poller, start_link, ["https://api.github.com/events", default_score_fun()]} }
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.

default_score_fun() ->
	fun(Events) ->
		lists:map(fun increment_user/1, Events)
	end.

increment_user({User, EventName}) ->
	Increment = get_increment(EventName),
	github_score_datastore:increment_user(User, Increment).

get_increment(<<"PushEvent">>) -> 5;
get_increment(<<"PullRequestReviewCommentEvent">>) -> 4;
get_increment(<<"WatchEvent">>) -> 3;
get_increment(<<"CreateEvent">>) -> 2;
get_increment(_) -> 1.
