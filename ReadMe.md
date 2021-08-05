GitHub Score
=====

A score card system for users.

Written in Erlang to speed delivery of the project, though the fundamentals
should be the same if written in Elixir.

Building and Running
=====

This is in process of being implemented in elixir. To run the incomlete elixir version,
use `mix` or `mix test` for the tests.

Building requires git, erlang 22.1 or newer, and make.

## basic build:

    make

## run tests:

dialyzer was not used as building the plt was time I did not yet want to spend.
However, there are unit tests at least:

    make tests

## run a development server on port 7654:

The command below will start and interactive erlang session, and start the
server running on port 7654.

    make shell

Usage
=====

To query the server, point a curl request (or browser) to the server and ask
for the user id you wish to know about:

    curl "http://localhost:7654/scores/97"

This will likely return a "0" as no user has been scrapped and imported yet. To
get some data, you can, on the erlang shell, add some:

    github_score_datastore:increment_user(97, 5).

The next time you curl above, you will get "5". If you repeat the same increment,
the next curl request will be "10", as it keeps a running tally.

Improvements
=====

This is primarily a proof of concept, and not what I would deliver as a final
product. It's close, but needs at least another day of polish. The following
is missing:

* **Logging**. Currently only otp events are logged, and only to the console. This
makes it unsuitable for release, but adding logging using the existing erlang
logger module is trivial. Mainly this is creating a reasonable default.
* **Configuration**. The port, github url, and function that populates the datastore
from the poller are all hard-coded. While the last is likely to stay that way,
it should be moved out of the supervisor and into some module that the supervisor
can then reference. The others are obvious on why they should be configurable.
* **Dialyzer**. While I've attempted to keep the types clean and documented, there
is no replacement for a tool. Yet another relatively simple thing to add.
* **Data storage**. The datastore is ets, which is memory only. Even a simple
switch to using dets, the disk backed version of ets, would be an improvement.
For the purpose of the project as presented, anything more is likely overkill.
Fortuately, datastore does not expose an underlying implementation, so swapping
that is an isolated change.
* **Improved Polling**. The polling as implemented is very naive. It doesn't
handle pagination, primarily, and so could lose or miss data. This would need to
be shored up.
* **Webhook**. Polling was the simplest thing that could work, and may end up
being all we need. This is more of a feature that could be revisited than a
hard requirement for me to be satisfied the job is "done".
* **Documentation**. I never know if the documentation I've provided is enough
or fully correct.

Some of this would come out of code review, some from ci, and some from personal
pride. But in the interest of time, I've decided to present what I have done,
and present what I would do if this was a real project.
