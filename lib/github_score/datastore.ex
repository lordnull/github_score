defmodule GithubScore.Datastore do

    def start_link() do
        try do
            :ets.new(:github_score_datastore, [ :named_table, :set, :public])
        rescue
            ArgumentError ->
                :ok
        end
        :ignore
    end

    def child_spec([]) do
        %{id: GithubScore.Datastore, start: {GithubScore.Datastore, :start_link, []}}
    end

    def increment_score(user, amount) do
        n = :ets.update_counter(:github_score_datastore, user, {2, amount}, {user, 0})
        {:ok, n}
    end

    def get_score(user) do
        try do
            case :ets.lookup_element(:github_score_datastore, user, 2) do
                n -> {:ok, n}
            end
        rescue
            ArgumentError ->
                {:ok, 0}
        end
    end

end
