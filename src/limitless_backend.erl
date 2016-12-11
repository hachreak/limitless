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
%%% @doc limitless API backend.
%%% @end

-module(limitless_backend).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

%% API

-export([
  bulk_read/2,
  create/6,
  delete/2,
  drop/1,
  extra_info/1,
  dec/2,
  init/1,
  is_reached/2,
  next_id/1,
  reset_expired/2
]).

%% Types

-export_type([
  ctx/0,
  id/0,
  limit/0,
  objectid/0
]).

-type ctx()         :: map().
-type id()          :: any().
-type limit()       :: map().
-type limit_info()  :: {binary(), non_neg_integer(),
                        non_neg_integer(), non_neg_integer()}.
-type limits_info() :: list(limit_info()).
-type objectid()    :: any().
-type operator()    :: '$lt' | '$gt'.
-type proplist()    :: list({atom(), any()}).
-type timestamp()   :: erlang:timestamp().

%% API

-callback handle_bulk_read(objectid(), ctx()) -> list(limit()).

-callback handle_bulk_read(
      objectid(), operator(), timestamp(), ctx()) -> list(limit()).

-callback handle_create(limit(), ctx()) -> {ok, limit()} | {error, term()}.

-callback handle_delete(id(), ctx()) -> ok | {error, term()}.

-callback handle_drop(ctx()) -> ok | {error, term()}.

-callback handle_dec(objectid(), ctx()) -> ok.

-callback handle_init(proplist()) -> {ok, ctx()}.

-callback handle_next_id(ctx()) -> {ok, id()} | {error, term()}.

-callback handle_reset(id(), timestamp(), non_neg_integer(), ctx()) -> ok.

%% API

-spec init(list()) -> {ok, ctx()}.
init(BackendConfig) ->
  Backend = limitless_utils:get_or_fail(name, BackendConfig),
  Configs = limitless_utils:get_or_fail(config, BackendConfig),
  {ok, BackendCtx} = Backend:handle_init(Configs),
  {ok, #{backend => Backend, backendctx => BackendCtx}}.

-spec next_id(ctx()) -> {ok, id()} | {error, term()}.
next_id(#{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_next_id(BackendCtx).

-spec create(binary(), id(), objectid(), non_neg_integer(),
             non_neg_integer(), ctx()) -> {ok, limit()} | {error, term()}.
create(Type, Id, ObjectId, Frequency, MaxRequests,
       #{backend := Backend, backendctx := BackendCtx}) ->
  Limit =  #{
    <<"_id">> => Id,
    <<"type">> => Type,
    <<"objectid">> => ObjectId,
    <<"frequency">> => Frequency,
    <<"max">> => MaxRequests,
    <<"current">> => MaxRequests,
    <<"expiry">> => reset(Frequency)
   },
  Backend:handle_create(Limit, BackendCtx).

-spec delete(id(), ctx()) -> ok | {error, term()}.
delete(Id, #{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_delete(Id, BackendCtx).

-spec drop(ctx()) -> ok | {error, term()}.
drop(#{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_drop(BackendCtx).

-spec bulk_read(objectid(), ctx()) -> list(limit()).
bulk_read(ObjectId, #{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_bulk_read(ObjectId, BackendCtx).

% @doc Reset all expired limiter.
% @end
-spec reset_expired(objectid(), ctx()) -> ok.
reset_expired(ObjectId, #{backend := Backend, backendctx := BackendCtx}) ->
  Limits = Backend:handle_bulk_read(
             ObjectId, '$lt', erlang:timestamp(), BackendCtx),
  lists:foreach(fun(#{<<"_id">> := Id, <<"frequency">> := Frequency,
                      <<"max">> := MaxRequests}) ->
      Backend:handle_reset(Id, reset(Frequency), MaxRequests, BackendCtx)
    end, Limits).

-spec dec(objectid(), ctx()) -> ok.
dec(ObjectId, #{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_dec(ObjectId, BackendCtx).

% @doc Check if all limits are not reached.
% @end
-spec is_reached(objectid(), ctx()) -> {boolean(), list(limit())}.
is_reached(ObjectId, #{backend := Backend, backendctx := BackendCtx}) ->
  Limits = Backend:handle_bulk_read(
           ObjectId, '$gt', erlang:timestamp(), BackendCtx),
  IsReached = lists:any(
    fun(#{<<"current">> := Current}) ->
        check(Current)
    end, Limits),
  {IsReached, Limits}.

-spec extra_info(list(limit())) -> limits_info().
extra_info(Limits) ->
  Now = limitless_utils:timestamp_to_gregorian_seconds(erlang:timestamp()),
  lists:map(
    fun(#{<<"current">> := Current, <<"max">> := Max,
          <<"expiry">> := End, <<"type">> := Type}) ->
        WhenReset = limitless_utils:timestamp_to_gregorian_seconds(End) - Now,
        {Type, Max, remaining(Current), WhenReset}
    end, Limits).

%% Private functions

-spec remaining(non_neg_integer()) -> non_neg_integer().
remaining(Current) when Current =< 0 -> 0;
remaining(Current) -> Current.

-spec check(integer()) -> boolean().
check(Current) ->
  Current =< 0.

% @doc Compute next interval computed as `now + frequency in seconds`.
% @end
-spec reset(non_neg_integer()) -> timestamp().
reset(Frequency) ->
  limitless_utils:gregorian_second2timestamp(
    limitless_utils:timestamp_to_gregorian_seconds(
      erlang:timestamp()) + Frequency).
