%%% @doc A process that periodically polls the github events api, and then
%%% shoves the desired data into the github_score_datastore.
%%% The primary guide on how this was designed and the use of the github api is
%%% at: https://docs.github.com/en/rest/reference/activity#events
-module(github_score_poller).
-behaivor(gen_server).

-export([start_link/1, start_link/2, next_poll_in/0, force_poll/0]).
-export([init/1, terminate/2, code_change/3,
	handle_call/3, handle_cast/2, handle_info/2]).

%% @doc The same as start_link/1 with the url format of
%% "https://api.github.com/events".
-spec start_link(fun(([ {binary(), binary()} ]) -> ok)) -> {ok, pid()}.
start_link(PollAction) ->
	start_link("https://api.github.com/events", PollAction).

%% @doc Start a poller that when given a user, will query a formatted string
%% as the url. The url format must include exaclty one "~s" to represent a user's
%% identifier.
-spec start_link(UrlFormat :: string(), fun(({binary(), binary()}) -> ok)) -> {ok, pid()}.
start_link(UrlFormat, PollAction) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE,
		#{url => UrlFormat, poll_action => PollAction}, []).

%% @doc An inspection function to see how long before the poll time will be.
-spec next_poll_in() -> timer:timeout().
next_poll_in() ->
	gen_server:call(?MODULE, next_poll_in, infinity).

%% @doc Rather than waiting for the next poll, force one to happen now. This
%% will reset the poll timer.
-spec force_poll() -> ok.
force_poll() ->
	gen_server:cast(?MODULE, poll).

% gen_server callbacks.

-record(state, {
	poll_timer,
	url = "http://localhost:8080/",
	poll_action = fun(_) -> ok end,
	last_etag = "",
	poll_interval = 60000
}).

init(#{ url := Url, poll_action := Callback} ) ->
	Timer = erlang:send_after(1000, self(), poll),
	State = #state{ poll_timer = Timer, url = Url , poll_action = Callback},
	{ok, State}.

handle_call(next_poll_in, _From, State) ->
	case State#state.poll_timer of
		undefined ->
			{reply, never, State};
		T ->
			case erlang:read_timer(T) of
				false ->
					{reply, never, State};
				N ->
					{reply, N / 1000, State}
			end
	end;

handle_call(_Msg, _From, State) ->
	{reply, {error, invalid}, State}.

handle_cast(poll, State) ->
	NewState = do_poll(State),
	{noreply, NewState};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(poll, State) ->
	NewState = do_poll(State),
	{noreply, NewState}.

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

terminate(_Why, _State) -> ok.

do_poll(State) ->
	State1 = cancel_timer(State),
	Headers = case State1#state.last_etag of
		undefined ->
			[ {"accept", "application/vnd.github.v3+json"} ];
		Etag ->
			[ {"If-None-Match", Etag}, {"accept", "application/vnd.github.v3+json"} ]
	end,
	Request = {State1#state.url, Headers},
	HttpResponse = httpc:request(get, Request, [], []),
	State2 = handle_http_response(HttpResponse, State1),
	start_timer(State2).

handle_http_response({ok, {{_, 200, _}, Headers, Body}}, State) ->
	BodyBin = list_to_binary(Body),
	Json = jsx:decode(BodyBin, [return_maps]),
	Events = lists:map(fun decode_event/1, Json),
	PollAction = State#state.poll_action,
	_ = PollAction(Events),
	NextPollTime = next_poll_time(Headers),
	Etag = proplists:get_value("ETag", Headers),
	State#state{ poll_interval = NextPollTime, last_etag = Etag};
handle_http_response({ok, {{_, 304, _}, Headers, _}}, State) ->
	NextPollTime = next_poll_time(Headers),
	Etag = proplists:get_value("ETag", Headers),
	State#state{ poll_interval = NextPollTime, last_etag = Etag };
handle_http_response(Failed, State) ->
	io:format("Failed to do a poll; previous poll interval will be retained: ~p", [Failed]),
	State.

next_poll_time(Headers) ->
	FixedHeaders = downcase_headers(Headers),
	NextPollTimeStr = proplists:get_value("x-poll-interval", FixedHeaders, "undefined"),
	try erlang:list_to_integer(NextPollTimeStr) of
		N -> N * 1000
	catch
		error:badarg ->
			infinity
	end.

downcase_headers(Headers) ->
	[ { string:to_lower(F), V} || {F, V} <- Headers ].

cancel_timer(#state{ poll_timer = undefined} = State) ->
	State;
cancel_timer(State) ->
	_ = erlang:cancel_timer(State#state.poll_timer),
	State#state{ poll_timer = undefined }.

start_timer(#state{ poll_interval = infinity} = State) ->
	State;
start_timer(State) ->
	Interval = State#state.poll_interval,
	Timer = erlang:send_after(Interval, self(), poll),
	State#state{ poll_timer = Timer }.

decode_event(#{ <<"type">> := Type, <<"actor">> := Actor}) ->
	#{ <<"id">> := Id} = Actor,
	{Id, Type}.


