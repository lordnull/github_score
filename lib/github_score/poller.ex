defmodule GithubScore.Poller do
    use GenServer


    def start_link([url, pollaction]) do
        GenServer.start_link(GithubScore.Poller, [url, pollaction], name: GithubScore.Poller)
    end

    def next_poll_in() do
        GenServer.call(GithubScore.Poller, :next_poll_in, :infinity)
    end

    def force_poll() do
        GenServer.cast(GithubScore.Poller, :poll)
    end

    @impl true
    def init([url, pollaction]) do
        timer = Process.send_after(Kernel.self, :poll, 1000)
        {:ok, %{
            poll_timer: timer,
            url: url,
            poll_action: pollaction,
            poll_interval: :infinity,
            last_etag: ""}}
    end

    @impl true
    def handle_call(:next_poll_in, _, state) do
        case state[:poll_timer] do
            nil ->
                {:reply, :never, state}
            timer ->
                case Process.read_timer(timer) do
                    false ->
                        {:reply, :never, state}
                    n ->
                        {:reply, n / 1000, state}
                end
        end
    end

    @impl true
    def handle_cast(:poll, state) do
        new_state = do_poll(state)
        {:noreply, new_state}
    end

    @impl true
    def handle_info(:poll, state) do
        new_state = do_poll(state)
        {:noreply, new_state}
    end

    defp do_poll(state) do
        state
    end

end
#
#code_change(_Vsn, State, _Extra) ->
#	{ok, State}.
#
#terminate(_Why, _State) -> ok.
#
#do_poll(State) ->
#	State1 = cancel_timer(State),
#	Headers = case State1#state.last_etag of
#		undefined ->
#			[ {"accept", "application/vnd.github.v3+json"} ];
#		Etag ->
#			[ {"If-None-Match", Etag}, {"accept", "application/vnd.github.v3+json"} ]
#	end,
#	Request = {State1#state.url, Headers},
#	HttpResponse = httpc:request(get, Request, [], []),
#	State2 = handle_http_response(HttpResponse, State1),
#	start_timer(State2).
#
#handle_http_response({ok, {{_, 200, _}, Headers, Body}}, State) ->
#	BodyBin = list_to_binary(Body),
#	Json = jsx:decode(BodyBin, [return_maps]),
#	Events = lists:map(fun decode_event/1, Json),
#	PollAction = State#state.poll_action,
#	_ = PollAction(Events),
#	NextPollTime = next_poll_time(Headers),
#	Etag = proplists:get_value("ETag", Headers),
#	State#state{ poll_interval = NextPollTime, last_etag = Etag};
#handle_http_response({ok, {{_, 304, _}, Headers, _}}, State) ->
#	NextPollTime = next_poll_time(Headers),
#	Etag = proplists:get_value("ETag", Headers),
#	State#state{ poll_interval = NextPollTime, last_etag = Etag };
#handle_http_response(Failed, State) ->
#	io:format("Failed to do a poll; previous poll interval will be retained: ~p", [Failed]),
#	State.
#
#next_poll_time(Headers) ->
#	FixedHeaders = downcase_headers(Headers),
#	NextPollTimeStr = proplists:get_value("x-poll-interval", FixedHeaders, "undefined"),
#	try erlang:list_to_integer(NextPollTimeStr) of
#		N -> N * 1000
#	catch
#		error:badarg ->
#			infinity
#	end.
#
#downcase_headers(Headers) ->
#	[ { string:to_lower(F), V} || {F, V} <- Headers ].
#
#cancel_timer(#state{ poll_timer = undefined} = State) ->
#	State;
#cancel_timer(State) ->
#	_ = erlang:cancel_timer(State#state.poll_timer),
#	State#state{ poll_timer = undefined }.
#
#start_timer(#state{ poll_interval = infinity} = State) ->
#	State;
#start_timer(State) ->
#	Interval = State#state.poll_interval,
#	Timer = erlang:send_after(Interval, self(), poll),
#	State#state{ poll_timer = Timer }.
#
#decode_event(#{ <<"type">> := Type, <<"actor">> := Actor}) ->
#	#{ <<"id">> := Id} = Actor,
#	{Id, Type}.


