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
  create/5,
  delete/2,
  drop/1,
  inc/2,
  init/0,
  is_reached/2,
  next_id/1,
  reset_expired/2
]).

%% Types

-export_type([
  appctx/0,
  id/0,
  limit/0,
  objectid/0,
  timestamp/0
]).

-type appctx()    :: any().
-type id()        :: any().
-type limit()     :: map().
-type objectid()  :: any().
-type operator()  :: '$lt' | '$gt'.
-type proplist()  :: list({atom(), any()}).
% -type timestamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type timestamp() :: erlang:timestamp().

%% API

-callback handle_bulk_read(objectid(), appctx()) -> list(limit()).

-callback handle_bulk_read(
      objectid(), operator(), timestamp(), appctx()) -> list(limit()).

-callback handle_create(limit(), appctx()) -> ok | {error, term()}.

-callback handle_delete(id(), appctx()) -> ok | {error, term()}.

-callback handle_drop(appctx()) -> ok | {error, term()}.

-callback handle_inc(objectid(), appctx()) -> ok.

-callback handle_init(proplist()) -> appctx().

-callback handle_next_id(appctx()) -> {ok, id()} | {error, term()}.

-callback handle_reset(id(), timestamp(), non_neg_integer(), appctx()) -> ok.

%% API

-spec init() -> {ok, appctx()}.
init() ->
  {ok, BackendConfig} = application:get_env(limitless, backend),
  Backend = limitless_utils:get_or_fail(name, BackendConfig),
  Configs = limitless_utils:get_or_fail(config, BackendConfig),
  {ok, BackendCtx} = Backend:handle_init(Configs),
  {ok, #{backend => Backend, backendctx => BackendCtx}}.

-spec next_id(appctx()) -> {ok, id()} | {error, term()}.
next_id(#{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_next_id(BackendCtx).

-spec create(
    id(), objectid(), non_neg_integer(), non_neg_integer(), appctx()) ->
      {ok, limit()}.
create(Id, ObjectId, Frequency, MaxRequests,
       #{backend := Backend, backendctx := BackendCtx}) ->
  Limit =  #{
    <<"_id">> => Id,
    <<"objectid">> => ObjectId,
    <<"frequency">> => Frequency,
    <<"max">> => MaxRequests,
    <<"current">> => 0,
    <<"expiry">> => erlang:timestamp()
   },
  Backend:handle_create(Limit, BackendCtx).

-spec delete(id(), appctx()) -> ok | {error, term()}.
delete(Id, #{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_delete(Id, BackendCtx).

-spec drop(appctx()) -> ok | {error, term()}.
drop(#{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_drop(BackendCtx).

-spec bulk_read(objectid(), appctx()) -> list(limit()).
bulk_read(ObjectId, #{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_bulk_read(ObjectId, BackendCtx).

% @doc Reset all expired limiter.
% @end
-spec reset_expired(objectid(), appctx()) -> ok.
reset_expired(ObjectId, #{backend := Backend, backendctx := BackendCtx}) ->
  Limits = Backend:handle_bulk_read(
             ObjectId, '$lt', erlang:timestamp(), BackendCtx),
  lists:foreach(fun(#{<<"_id">> := Id, <<"frequency">> := Frequency}) ->
      Backend:handle_reset(Id, reset(Frequency), 0, BackendCtx)
    end, Limits).

-spec inc(objectid(), appctx()) -> ok.
inc(ObjectId, #{backend := Backend, backendctx := BackendCtx}) ->
  Backend:handle_inc(ObjectId, BackendCtx).

% @doc Check if all limits are not reached.
% @end
-spec is_reached(objectid(), appctx()) -> boolean().
is_reached(ObjectId, #{backend := Backend, backendctx := BackendCtx}) ->
  lists:any(
    fun(#{<<"current">> := Current, <<"max">> := Max}) ->
        Current > Max
    end, Backend:handle_bulk_read(
           ObjectId, '$gt', erlang:timestamp(), BackendCtx)).

%% Private functions

% @doc Compute next interval computed as `now + frequency in seconds`.
% @end
-spec reset(non_neg_integer()) -> timestamp().
reset(Frequency) ->
  gregorian_second2timestamp(
    timestamp_to_gregorian_seconds(erlang:timestamp()) + Frequency).

% @doc Computes the number of gregorian seconds starting with year 0 and
% ending at the specified timestamp.
% @end
-spec timestamp_to_gregorian_seconds(timestamp()) -> non_neg_integer().
timestamp_to_gregorian_seconds(Timestamp) ->
  calendar:datetime_to_gregorian_seconds(
    calendar:now_to_universal_time(Timestamp)).

% @doc Convert the number of gregorian seconds in a timestamp.
% @end
-spec gregorian_second2timestamp(non_neg_integer()) -> timestamp().
gregorian_second2timestamp(Seconds) ->
  {Seconds div 1000000, Seconds rem 1000000, 0}.
