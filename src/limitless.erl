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
  create/6,
  init/0,
  is_reached/2,
  next_id/1
]).

%% Types

-type appctx()     :: limitless_backend:appctx().
-type limit_info() :: limitless_backend:limit_info().
-type objectid()   :: limitless_backend:objectid().
-type id()         :: limitless_backend:id().
-type limit()      :: limitless_backend:limit().


%%====================================================================
%% API
%%====================================================================

-spec init() -> {ok, appctx()}.
init() ->
  limitless_backend:init().

-spec is_reached(objectid(), appctx()) -> {boolean(), list(limit_info())}.
is_reached(ObjectId, AppCtx) ->
  limitless_backend:reset_expired(ObjectId, AppCtx),
  {IsReached, Limits} = conditional_inc(
      limitless_backend:is_reached(ObjectId, AppCtx), ObjectId, AppCtx),
  ExtraInfo = limitless_backend:extra_info(Limits),
  {IsReached, ExtraInfo}.

-spec next_id(appctx()) -> {ok, id()} | {error, term()}.
next_id(AppCtx) ->
  limitless_backend:next_id(AppCtx).

-spec create(binary(), id(), objectid(), non_neg_integer(),
             non_neg_integer(), appctx()) -> {ok, limit()} | {error, term()}.
create(Type, Id, ObjectId, Frequency, MaxRequests, AppCtx) ->
  limitless_backend:create(Type, Id, ObjectId, Frequency, MaxRequests, AppCtx).

%%====================================================================
%% Internal functions
%%====================================================================

-spec conditional_inc({boolean(), list(limit())}, objectid(), appctx()) ->
    {boolean(), list(limit())}.
conditional_inc({true, Limits}, _, _) -> {true, Limits};
conditional_inc({false, Limits}, ObjectId, AppCtx) ->
  limitless_backend:inc(ObjectId, AppCtx),
  {false, Limits}.
