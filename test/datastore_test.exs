defmodule GithubScore.DatastoreTest do
  use ExUnit.Case
  alias GithubScore.Datastore, as: Datastore

  test "simple usages" do
    :ignore = Datastore.start_link()

    assert({:ok, 0} === Datastore.get_score("no user"))
    assert({:ok, 1} === Datastore.increment_score("user a", 1))
    assert({:ok, 0} === Datastore.get_score("no user"))
    assert({:ok, 5} === Datastore.increment_score("user a", 4))
    assert({:ok, 5} === Datastore.get_score("user a"))
    assert({:ok, 27} === Datastore.increment_score("user b", 27))
    assert({:ok, 5} === Datastore.get_score("user a"))
  end

end
