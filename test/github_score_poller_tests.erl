-module(github_score_poller_tests).

-include_lib("eunit/include/eunit.hrl").

polling_test_() ->
	{setup, fun() ->
		{ok, ?MODULE} = fake_github:start_server(?MODULE),
		fake_github:get_url(?MODULE)
	end,
	fun(_) ->
		fake_github:stop(?MODULE)
	end,
	fun(Url) ->
		{setup, local,
		fun() ->
			Self = self(),
			CallbackFun = fun(Events) ->
				Self ! {events, Events}
			end,
			{ok, Pid} = github_score_poller:start_link(Url, CallbackFun),
			Pid
		end,
		[ fun() ->
			ExpectedEvents = [
				{<<"billy">>, <<"e1">>},
				{<<"joe">>, <<"e3">>}
			],
			ok = fake_github:set_events(?MODULE, ExpectedEvents),
			ok = github_score_poller:force_poll(),
			Events = receive
				{events, E} -> E
			end,
			?assertEqual(lists:sort(ExpectedEvents), lists:sort(Events)),
			NextPollIn = github_score_poller:next_poll_in(),
			?assertNotEqual(infinity, NextPollIn),
			?assert(NextPollIn > 0)
		end
		, fun() ->
			ExpectedEvents = [
				{<<"jay">>, <<"e4">>},
				{<<"jones">>, <<"e5">>}
			],
			ok = fake_github:set_events(?MODULE, ExpectedEvents),
			Events = receive
				{events, E} -> E
			after
				5000 ->
					error(github_not_polled)
			end,
			?assertEqual(lists:sort(ExpectedEvents), lists:sort(Events))
		end
		] }
	end }.
