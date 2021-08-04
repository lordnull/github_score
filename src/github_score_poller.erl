%%% @doc A process that periodically polls the github events api, and then
%%% shoves the desired data into the github_score_datastore.
%%% The primary guide on how this was designed and the use of the github api is
%%% at: https://docs.github.com/en/rest/reference/activity#events
-module(github_score_poller).
-behaivor(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, terminate/2, code_change/3,
	handle_call/3, handle_cast/2, handle_info/2]).

%% @doc The same as start_link/1 with the url format of
%% "https://api.github.com/users/~s/events".
-spec start_link() -> {ok, pid()}.
start_link() ->
	start_link("https://api.github.com/users/~s/events").

%% @doc Start a poller that when given a user, will query a formatted string
%% as the url. The url format must include exaclty one "~s" to represent a user's
%% identifier.
-spec start_link(UrlFormat :: string()) -> {ok, pid()}.
start_link(UrlFormat) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, #{url_format => UrlFormat}, []).

% gen_server callbacks.

-record(state, {
	poll_timer,
	url_format,
	last_etag = "",
	poll_interval = 60
}).

init(#{ url_format := Format} ) ->
	{ok, Timer} = timer:send_after(1000, initial_poll),
	State = #state{ poll_timer = Timer, url_format = Format },
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
