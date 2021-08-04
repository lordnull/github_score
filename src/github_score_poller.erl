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
	gen_server:cast(?MODULE, force_poll, infinity).

% gen_server callbacks.

-record(state, {
	poll_timer,
	url = "http://localhost:8080/",
	poll_action = fun(_) -> ok end,
	last_etag = "",
	poll_interval = 60
}).

init(#{ url := Url, poll_action := Callback} ) ->
	{ok, Timer} = timer:send_after(1000, initial_poll),
	State = #state{ poll_timer = Timer, url = Url , poll_action = Callback},
	{ok, State}.

handle_call(_Msg, _From, State) ->
	{reply, {error, invalid}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(initial_poll, State) ->
	{noreply, State}.

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

terminate(_Why, _State) -> ok.
