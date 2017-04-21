%%%-------------------------------------------------------------------
%% @doc example_mongodb_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(example_mongodb_backend_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ValidTokens = [<<"token1">>, <<"token2">>],
  {ok, LimitlessCtx} = limitless:init(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", homepage_handler, {ValidTokens, LimitlessCtx}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}]}
  ]),
  example_mongodb_backend_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
