%%% @doc It's genrally a bad idea to have our tests rely on an external serverice,
%%% So this exists to act like a github equivalent, allowing one to determine
%%% the events and such for a user.
-module(fake_github).
% while this claims to be a gen_server, I'm not declaring it as such because
% I don't want the missing callbacks to be called out by the compiler.

% The event name can be an arbitary binary because we want to have a 'catchall'
% The non-catchall event names are:
% * PushEven
% * PullRequestReviewCommentEvent
% * WatchEvent
% * CreateEvent
-type event_name() :: binary().
-type user_identifier() :: binary().

-record(state, {
	events = [] :: {user_identifier(), event_name()},
	port = 0
	}).

% gen_server-esque
-export([init/1, handle_call/3, handle_cast/2]).
% cowboy as it needs to be a request handler.
-export([init/2]).
% data and "system" management.
-export([start_server/1, stop_server/1]).
-export([get_url/1, set_events/2, get_events/1]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Start the cowboy listener and the fake github datastore. Name is used
%% both for the cowboy listener and the server name; best practice is for it
%% to be the same as the test fixture/suite it's for. This will start cowboy
%% if not already done so.
start_server(Name) ->
	inets:start(),
	% cowboy doesn't let up do port 0 for a random port, so we'll get it
	% another way.
	{ok, P} = gen_tcp:listen(0, []),
	{ok, Port} = inet:port(P),
	ok = gen_tcp:close(P),
	Dispatch = cowboy_router:compile([
		{'_', [{"/", ?MODULE, Name}]}
	]),
	{ok, _} = application:ensure_all_started(cowboy),
	{ok, _} = cowboy:start_clear(Name,
		[{port, Port}],
		#{ env => #{ dispatch => Dispatch }}),
	{ok, _} = gen_server:start({local, Name}, ?MODULE, #{ port => Port }, []),
	{ok, Name}.

stop_server(Name) ->
	ok = gen_server:cast(Name, stop),
	_ = spawn(fun() ->
		ok = cowboy:stop_listener(Name)
	end),
	ok.

get_url(Name) ->
	gen_server:call(Name, get_url_format, infinity).

set_events(Name, Events) ->
	gen_server:cast(Name, {set_events, Events}).

get_events(Name) ->
	gen_server:call(Name, get_events, infinity).

% gen_server-esque

init(#{ port := Port}) ->
	{ok, #state{ port = Port }}.

handle_call(get_events, _From, State) ->
	{reply, State#state.events, State#state{ events = []}};
handle_call(get_url_format, _From, State) ->
	Port = State#state.port,
	UrlIo = io_lib:format("http://localhost:~p/", [Port]),
	{reply, binary_to_list(iolist_to_binary(UrlIo)), State}.

handle_cast({set_events, Events}, State) ->
	{noreply, State#state{events = Events}};

handle_cast(stop, State) ->
	{stop, normal, State}.


init(Req, Name) ->
	Events = get_events(Name),
	Encoded = encode_events(Events),
	Reply = cowboy_req:reply(200,
		#{ <<"content-type">> => <<"application/vnd.github.v3+json">>
		,  <<"etag">> => <<"1">>
		,  <<"X-Poll-Interval">> => <<"3">>}, Encoded, Req),
	{ok, Reply, Name}.

encode_events(Events) ->
	jsx:encode(lists:map(fun encode_event/1, Events)).

encode_event({User, Event}) ->
	#{ type => Event, actor => #{ id => User }}.

