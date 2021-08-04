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

increment_score(User, Amount) ->
	throw(nyi).

get_score(User) ->
	throw(nyi).

-ifdef(TEST).

simple_test_() ->
	{setup, fun() ->
		ignore = ?MODULE:start_link()
	end,
	[ ?_assertEqual({ok, 0}, ?MODULE:get_score(<<"no user">>))
	, ?_assertEqual({ok, 1}, ?MODULE:incement_score(<<"user a">>))
	, ?_assertEqual({ok, 0}, ?MODULE:get_score(<<"no user">>))
	, ?_assertEqual({ok, 5}, ?MODULE:increment_score(<<"user a">>))
	, ?_assertEqual({ok, 5}, ?MODULE:get_score(<<"user a">>))
	, ?_assertEqual({ok, 27}, ?MODULE:increment_score(<<"user b">>))
	, ?_assertEqual({ok, 5}, ?MODULE:get_score(<<"user a">>))
	] }.

-endif.
