%%%-------------------------------------------------------------------
%% @doc example_minidb_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(example_minidb_backend_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ValidTokens = [<<"token1">>, <<"token2">>],
  {ok, LimitlessCtx} = limitless:init(),
  fixtures_tokens(LimitlessCtx),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", homepage_handler, {ValidTokens, LimitlessCtx}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}]}
  ]),
  example_minidb_backend_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

fixtures_tokens(LimitlessCtx) ->
  %% Configure the limits for our two tokens:
  limitless:setup(<<"token1">>, token, LimitlessCtx),
  limitless:setup(<<"token2">>, token, LimitlessCtx).
