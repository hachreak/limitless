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
%%% @doc limitless API Backend MongoDB (mongopool)
%%% @end

-module(limitless_backend_mongopool).

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

-type appctx()    :: #{pool => atom(), table => atom()}.
-type id()        :: limitless_backend:id().
-type limit()     :: limitless_backend:limit().
-type objectid()  :: limitless_backend:objectid().
-type operator()  :: limitless_backend:operator().
-type proplist()  :: limitless_backend:proplist().
-type timestamp() :: limitless_backend:timestamp().

%% API

-spec handle_create(limit(), appctx()) -> {ok, limit()} | {error, term()}.
handle_create(Limit, #{pool := Pool, table := Table}) ->
  mongopool_app:insert(Pool, Table, Limit),
  {ok, Limit}.

-spec handle_init(proplist()) -> {ok, appctx()}.
handle_init(Configs) ->
  application:ensure_all_started(mongopool),
  Pool = limitless_utils:get_or_fail(pool, Configs),
  Table = proplists:get_value(table, Configs, limitless),
  {ok, #{pool => Pool, table => Table}}.

-spec handle_delete(id(), appctx()) -> ok | {error, term()}.
handle_delete(Id, #{pool := Pool, table := Table}) ->
  mongopool_app:delete(Pool, Table, #{<<"_id">> => Id}),
  ok.

-spec handle_drop(appctx()) -> ok | {error, term()}.
handle_drop(#{pool := Pool, table := Table}) ->
  mongopool_app:delete(Pool, Table, #{}),
  ok.

-spec handle_bulk_read(objectid(), appctx()) -> list(limit()).
handle_bulk_read(ObjectId, AppCtx) ->
  get({'$and', [
       {<<"objectid">>, {'$eq', ObjectId}}
    ]}, AppCtx).

-spec handle_bulk_read(objectid(), operator(), timestamp(), appctx()) ->
    list(limit()).
handle_bulk_read(ObjectId, Operator, Expiry, AppCtx) ->
  get({'$and', [
       {<<"objectid">>, {'$eq', ObjectId}},
       {<<"expiry">>, {Operator, Expiry}}
    ]}, AppCtx).

% @doc decrement counter: add a new record {id, when}
% @end
-spec handle_dec(objectid(), appctx()) -> ok.
handle_dec(ObjectId, #{pool := Pool, table := Table}=AppCtx) ->
  lists:foreach(fun(#{<<"_id">> := Id}) ->
      mongopool_app:update(
        Pool, Table, #{<<"_id">> => Id},
        {<<"$inc">>, #{<<"current">> => -1}})
    end, handle_bulk_read(ObjectId, AppCtx)),
  ok.

% @doc Manually expiry old row than 'when'
% @end
-spec handle_reset(id(), timestamp(), non_neg_integer(), appctx()) -> ok.
handle_reset(Id, Expiry, Current, #{pool := Pool, table := Table}) ->
  mongopool_app:update(
    Pool, Table, #{<<"_id">> => Id},
    {<<"$set">>, #{<<"expiry">> => Expiry, <<"current">> => Current}}),
  ok.

%% Private functions

-spec get(tuple(), appctx()) -> list(limit()).
get(Query, #{pool := Pool, table := Table}) ->
  Cursor = mongopool_app:find(Pool, Table, Query),
  Limits = mc_cursor:take(Cursor, infinity),
  mc_cursor:close(Cursor),
  Limits.
