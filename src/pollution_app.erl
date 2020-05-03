%%%-------------------------------------------------------------------
%% @doc pollution public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_app).
-author("Samuel Heldak").

-behaviour(application).

-include_lib("eunit/include/eunit.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pollution_sup:start_link().

stop(_State) ->
    ok.
