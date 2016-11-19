%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2016 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc limitless public API
%%% @end

-module(limitless).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  init/0,
  is_reached/2,
  next_id/1,
  setup/3
]).

%% Types

-type appctx()     :: #{ctx => ctx(), limits => list()}.
-type ctx()        :: limitless_backend:ctx().
-type id()         :: limitless_backend:id().
-type limit()      :: limitless_backend:limit().
-type limit_info() :: limitless_backend:limit_info().
-type objectid()   :: limitless_backend:objectid().


%%====================================================================
%% API
%%====================================================================

-spec init() -> {ok, appctx()}.
init() ->
  {ok, BackendConfig} = application:get_env(limitless, backend),
  {ok, Ctx} = limitless_backend:init(BackendConfig),
  {ok, set_limits(application:get_env(limitless, limits), #{ctx => Ctx})}.

-spec is_reached(objectid(), appctx()) -> {boolean(), list(limit_info())}.
is_reached(ObjectId, #{ctx := Ctx}) ->
  limitless_backend:reset_expired(ObjectId, Ctx),
  {IsReached, Limits} = conditional_dec(
      limitless_backend:is_reached(ObjectId, Ctx), ObjectId, Ctx),
  ExtraInfo = limitless_backend:extra_info(Limits),
  {IsReached, ExtraInfo}.

-spec next_id(appctx()) -> {ok, id()} | {error, term()}.
next_id(#{ctx := Ctx}) ->
  limitless_backend:next_id(Ctx).

% @doc Setup limmits from configuration: initialize in database.
% @end
setup(ObjectId, Group, #{ctx := Ctx, limits := LimitsConfig}) ->
  lists:map(fun(Config) ->
      Type = limitless_utils:get_or_fail(type, Config),
      Frequency = limitless_utils:get_or_fail(frequency, Config),
      Requests = limitless_utils:get_or_fail(requests, Config),
      {ok, Id} = limitless_backend:next_id(Ctx),
      limitless_backend:create(
        Type, Id, ObjectId, Frequency, Requests, Ctx)
    end, proplists:get_value(Group, LimitsConfig)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec conditional_dec({boolean(), list(limit())}, objectid(), appctx()) ->
    {boolean(), list(limit())}.
conditional_dec({true, Limits}, _, _) -> {true, Limits};
conditional_dec({false, Limits}, ObjectId, Ctx) ->
  limitless_backend:dec(ObjectId, Ctx),
  {false, Limits}.

-spec set_limits(undefined | {ok, list()}, appctx()) -> appctx().
set_limits(undefined, AppCtx) -> AppCtx;
set_limits({ok, LimitsConfig}, AppCtx) -> AppCtx#{limits => LimitsConfig}.
