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
  is_reached/2
]).

%% Types

-type appctx()     :: limitless_backend:appctx().
-type limit_info() :: limitless_backend:limit_info().
-type objectid()   :: limitless_backend:objectid().


%%====================================================================
%% API
%%====================================================================

-spec init() -> {ok, appctx()}.
init() ->
  limitless_backend:init().

-spec is_reached(objectid(), appctx()) -> {boolean(), list(limit_info())}.
is_reached(ObjectId, AppCtx) ->
  limitless_backend:reset_expired(ObjectId, AppCtx),
  limitless_backend:inc(ObjectId, AppCtx),
  {IsReached, Limits} = limitless_backend:is_reached(ObjectId, AppCtx),
  ExtraInfo = limitless_backend:extra_info(Limits),
  {IsReached, ExtraInfo}.

%%====================================================================
%% Internal functions
%%====================================================================
