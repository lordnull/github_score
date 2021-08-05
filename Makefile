PROJECT = github_score
PROJECT_DESCRIPTION = Create a scorecard server based on github performance.
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsx
dep_cowboy_commit = 2.9.0

LOCAL_DEPS = inets

SHELL_NAME ?= github_score@127.0.0.1
SHELL_COOKIE ?= github_score
SHELL_OPTS = -eval 'application:ensure_all_started(github_score)' -name ${SHELL_NAME} -setcookie ${SHELL_COOKIE}


include erlang.mk
