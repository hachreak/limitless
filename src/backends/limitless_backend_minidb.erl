%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2017 Leonardo Rossi
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
%%% @doc limitless API Backend miniDB
%%% @end

-module(limitless_backend_minidb).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(limitless_backend).

-export([
  handle_bulk_read/2,
  handle_bulk_read/4,
  handle_create/2,
  handle_delete/2,
  handle_drop/1,
  handle_dec/2,
  handle_init/1,
  handle_reset/4
]).

%% Types

-type appctx()    :: #{}.
-type id()        :: limitless_backend:id().
-type limit()     :: limitless_backend:limit().
-type objectid()  :: limitless_backend:objectid().
-type operator()  :: limitless_backend:operator().
-type proplist()  :: limitless_backend:proplist().
-type timestamp() :: limitless_backend:timestamp().

%% API

-spec handle_create(limit(), appctx()) -> {ok, limit()} | {error, term()}.
handle_create(#{<<"_id">> := Id}=Limit, _AppCtx) ->
  minidb:put(Id, Limit),
  {ok, Limit}.

-spec handle_init(proplist()) -> {ok, appctx()}.
handle_init(_Configs) ->
  application:ensure_all_started(minidb),
  {ok, #{}}.

-spec handle_delete(id(), appctx()) -> ok | {error, term()}.
handle_delete(Id, _AppCtx) ->
  minidb:delete(Id),
  ok.

-spec handle_drop(appctx()) -> ok | {error, term()}.
handle_drop(_AppCtx) ->
  minidb:drop(),
  ok.

-spec handle_bulk_read(objectid(), appctx()) -> list(limit()).
handle_bulk_read(ObjectId, _AppCtx) ->
  minidb:find([{<<"objectid">>, {'$eq', ObjectId}}]).

-spec handle_bulk_read(objectid(), operator(), timestamp(), appctx()) ->
    list(limit()).
handle_bulk_read(ObjectId, Operator, Expiry, _AppCtx) ->
  minidb:find([
    {<<"objectid">>, {'$eq', ObjectId}},
    {<<"expiry">>, {Operator, Expiry}}
  ]).

% @doc decrement counter: add a new record {id, when}
% @end
-spec handle_dec(objectid(), appctx()) -> ok.
handle_dec(ObjectId, AppCtx) ->
  lists:foreach(fun(#{<<"_id">> := Id}) ->
      minidb:inc(Id, [{<<"current">>, -1}])
    end, handle_bulk_read(ObjectId, AppCtx)),
  ok.

% @doc Manually expiry old row than 'when'
% @end
-spec handle_reset(id(), timestamp(), non_neg_integer(), appctx()) -> ok.
handle_reset(Id, Expiry, Current, _AppCtx) ->
  minidb:patch(Id, #{<<"expiry">> => Expiry, <<"current">> => Current}),
  ok.

%% Private functions
