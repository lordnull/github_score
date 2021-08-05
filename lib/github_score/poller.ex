defmodule GithubScore.Poller do
    def child_spec(url) do
        %{ id: GithubScore.Poller, start: {GithubScore.Poller, :start_link, [url]}}
    end

    def start_link(_) do
        :ignore
    end

end
