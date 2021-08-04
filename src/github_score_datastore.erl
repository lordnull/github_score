%%% @doc The interface to the datastore where we're keeping user's scores.
-module(github_score_datastore).

-export([increment_score/2, get_score/1]).
-export([start_link/0]).

-ifdef(TEST).
% Eunit was chosen because it's a lighter and easier test framework, thus more
% suitable to smaller projects.
-include_lib("eunit/include/eunit.hrl").
-endif.

%%@doc this is to make it fit nicely into a supervisor tree, and have the supervisor
%% own the ets table we're using to store our, well, data.
%% ets is not the best choice because it's volatile memory, it's cleared out
%% if the supervisor exits (like when the vm stops). However, it's an easy api
%% and built-in, and means for quick dev I don't need to worry about cleanup for
%% filesystem stuff.
-spec start_link() -> ignore.
start_link() ->
	_Ets = ets:new(?MODULE, [public, named_table]),
	ignore.

%% @doc Given a user identifier and an amount, increase the datastore's track
%% of that user's score by the given amount. If the user is not yet tracked,
%% their score is set to the given amount.
-spec increment_score(User :: term(), integer()) -> {ok, integer()}.
increment_score(User, Amount) ->
	N = ets:update_counter(?MODULE, User, {2, Amount}, {User, 0}),
	{ok, N}.

%% @doc Fetch the score for a given user identifier. Users that have yet to be
%% tracked have a default score of 0.
-spec get_score(User :: term()) -> {ok, integer()}.
get_score(User) ->
	try ets:lookup_element(?MODULE, User, 2) of
		N -> {ok, N}
	catch
		error:badarg ->
			{ok, 0}
	end.

-ifdef(TEST).

simple_test_() ->
	{setup, local, fun() ->
		ignore = ?MODULE:start_link()
	end,
	[ ?_assertEqual({ok, 0}, ?MODULE:get_score(<<"no user">>))
	, ?_assertEqual({ok, 1}, ?MODULE:increment_score(<<"user a">>, 1))
	, ?_assertEqual({ok, 0}, ?MODULE:get_score(<<"no user">>))
	, ?_assertEqual({ok, 5}, ?MODULE:increment_score(<<"user a">>, 4))
	, ?_assertEqual({ok, 5}, ?MODULE:get_score(<<"user a">>))
	, ?_assertEqual({ok, 27}, ?MODULE:increment_score(<<"user b">>, 27))
	, ?_assertEqual({ok, 5}, ?MODULE:get_score(<<"user a">>))
	] }.

-endif.
