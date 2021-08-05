defmodule GithubScore do
  @moduledoc """
  Documentation for `GithubScore`.
  """
  use Application

  @doc """
  Hello world.

  ## Examples

      iex> GithubScore.hello()
      :world

  """

  @impl true
  def start(_type, _args) do
    GithubScore.Supervisor.start_link(name: GithubScore.Supervisor)
  end
end
