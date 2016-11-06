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
%%% @doc limitless API - tests.
%%% @end

-module(limitless_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

mongo_start() ->
  application:set_env(mongopool, pools,
    [
     {eshpool, [
                {size, 10},
                {max_overflow, 30}
               ], [
                   {database, <<"test-eunit-db">>},
                   {host, db}
                  ]}
    ]),
  application:set_env(
    limitless, backend, [
      {name, limitless_backend_mongopool},
      {config, [
        {table, limitless},
        {pool, eshpool}
      ]}
    ]),
  {ok, AppCtx} = limitless:init(),
  AppCtx.

mongo_stop(AppCtx) ->
  limitless_backend:drop(AppCtx),
  application:unset_env(mongopool, pools),
  application:unset_env(limitless_backend, backend).

fixture_limit_1() ->
  Id = <<"limit-1">>,
  ObjectId = <<"obj-1">>,
  Frequency = 3600,
  MaxRequests = 10,
  {Id, ObjectId, Frequency, MaxRequests}.

fixture_limit_2() ->
  Id = <<"limit-2">>,
  ObjectId = <<"obj-1">>,
  Frequency = 3600,
  MaxRequests = 20,
  {Id, ObjectId, Frequency, MaxRequests}.

fixture_limit_3() ->
  Id = <<"limit-3">>,
  ObjectId = <<"obj-2">>,
  Frequency = 3600,
  MaxRequests = 20,
  {Id, ObjectId, Frequency, MaxRequests}.

is_reached_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(AppCtx) -> [
        fun() ->
          {Id1, ObjectId1, Frequency1, MaxRequests1} = fixture_limit_1(),
          {Id2, ObjectId1, Frequency2, MaxRequests2} = fixture_limit_2(),
          {Id3, ObjectId3, Frequency3, MaxRequests3} = fixture_limit_3(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Id1, ObjectId1, Frequency1, MaxRequests1, AppCtx),
          {ok, _} = limitless_backend:create(
                    Id2, ObjectId1, Frequency2, MaxRequests2, AppCtx),
          {ok, _} = limitless_backend:create(
                    Id3, ObjectId3, Frequency3, MaxRequests3, AppCtx),
          % reach the limit for Id1
          lists:foreach(fun(_) ->
              ?assertEqual(false, limitless:is_reached(ObjectId1, AppCtx))
            end, lists:seq(1, MaxRequests1 + 1)),
          ?assertEqual(true, limitless:is_reached(ObjectId1, AppCtx)),
          ?assertEqual(false, limitless:is_reached(ObjectId3, AppCtx)),
          % check limit of objectid that doesn't exists
          ?assertEqual(false, limitless:is_reached(<<"doesnt-exist">>, AppCtx))
        end
      ]
    end
  }.
