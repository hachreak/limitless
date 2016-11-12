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
%%% @doc limitless API backend - tests.
%%% @end

-module(limitless_backend_tests).

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
  {ok, AppCtx} = limitless_backend:init(),
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

fixture_expired_limit_4() ->
  Id = <<"limit-4">>,
  ObjectId = <<"obj-1">>,
  Frequency = 1,
  MaxRequests = 10,
  {Id, ObjectId, Frequency, MaxRequests}.

create_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(AppCtx) -> [
        fun() ->
          {Id, ObjectId, Frequency, MaxRequests} = fixture_limit_1(),
          % check db empty
          [] = limitless_backend:bulk_read(ObjectId, AppCtx),
          % create limit
          {ok, Limit} = limitless_backend:create(
                    Id, ObjectId, Frequency, MaxRequests, AppCtx),
          % check returned value
          ?assertEqual(Id, maps:get(<<"_id">>, Limit)),
          ?assertEqual(ObjectId, maps:get(<<"objectid">>, Limit)),
          ?assertEqual(Frequency, maps:get(<<"frequency">>, Limit)),
          ?assertEqual(MaxRequests, maps:get(<<"max">>, Limit)),
          ?assertEqual(0, maps:get(<<"current">>, Limit)),
          ?assertEqual(true, maps:is_key(<<"expiry">>, Limit)),
          % check database
          [LimitWrote] = limitless_backend:bulk_read(ObjectId, AppCtx),
          ?assertEqual(Id, maps:get(<<"_id">>, LimitWrote)),
          ?assertEqual(ObjectId, maps:get(<<"objectid">>, LimitWrote)),
          ?assertEqual(Frequency, maps:get(<<"frequency">>, LimitWrote)),
          ?assertEqual(MaxRequests, maps:get(<<"max">>, LimitWrote)),
          ?assertEqual(0, maps:get(<<"current">>, LimitWrote)),
          ?assertEqual(true, maps:is_key(<<"expiry">>, LimitWrote))
        end
      ]
    end
  }.

delete_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(AppCtx) -> [
        fun() ->
          {Id1, ObjectId, Frequency1, MaxRequests1} = fixture_limit_1(),
          {Id2, ObjectId, Frequency2, MaxRequests2} = fixture_limit_2(),
          % check db empty
          [] = limitless_backend:bulk_read(ObjectId, AppCtx),
          % create limits
          {ok, _} = limitless_backend:create(
                    Id1, ObjectId, Frequency1, MaxRequests1, AppCtx),
          {ok, _} = limitless_backend:create(
                    Id2, ObjectId, Frequency2, MaxRequests2, AppCtx),
          % try to delete only the fixture 1
          limitless_backend:delete(Id1, AppCtx),
          % check db
          [Limit] = limitless_backend:bulk_read(ObjectId, AppCtx),
          ?assertEqual(Id2, maps:get(<<"_id">>, Limit))
        end
      ]
    end
  }.

inc_test_() ->
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
          % check objectids
          ?assertNotEqual(ObjectId1, ObjectId3),
          % try to increment current
          limitless_backend:inc(ObjectId1, AppCtx),
          % check db
          [Limit1, Limit2] = limitless_backend:bulk_read(ObjectId1, AppCtx),
          ?assertEqual(1, maps:get(<<"current">>, Limit1)),
          ?assertEqual(1, maps:get(<<"current">>, Limit2)),
          [Limit3] = limitless_backend:bulk_read(ObjectId3, AppCtx),
          ?assertEqual(0, maps:get(<<"current">>, Limit3))
        end
      ]
    end
  }.

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
          % reset timers
          limitless_backend:reset_expired(ObjectId1, AppCtx),
          limitless_backend:reset_expired(ObjectId3, AppCtx),
          % check if is reached (false)
          ?assertMatch({false, _},
                       limitless_backend:is_reached(ObjectId1, AppCtx)),
          ?assertMatch({false, _},
                       limitless_backend:is_reached(ObjectId3, AppCtx)),
          % reach the limit for Id3
          lists:foreach(fun(_) ->
              limitless_backend:inc(ObjectId3, AppCtx)
            end, lists:seq(1, MaxRequests3 + 1)),
          ?assertMatch({false, _},
                       limitless_backend:is_reached(ObjectId1, AppCtx)),
          ?assertMatch({true, _},
                       limitless_backend:is_reached(ObjectId3, AppCtx))
        end
      ]
    end
  }.

reset_expired_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(AppCtx) -> [
        fun() ->
          {Id1, ObjectId1, Frequency1,
           MaxRequests1} = fixture_expired_limit_4(),
          {Id3, ObjectId3, Frequency3, MaxRequests3} = fixture_limit_3(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Id1, ObjectId1, Frequency1, MaxRequests1, AppCtx),
          {ok, _} = limitless_backend:create(
                    Id3, ObjectId3, Frequency3, MaxRequests3, AppCtx),
          % increment counters
          limitless_backend:inc(ObjectId1, AppCtx),
          limitless_backend:inc(ObjectId3, AppCtx),
          % backup
          [#{<<"expiry">> := {_, Sec1, _},
             <<"current">> := 1}] = limitless_backend:bulk_read(
                                      ObjectId1, AppCtx),
          [#{<<"expiry">> := {_, Sec3, _},
             <<"current">> := 1}] = limitless_backend:bulk_read(
                                      ObjectId3, AppCtx),
          % reset timers after 3 seconds
          timer:sleep(3000),
          % this is resetted
          limitless_backend:reset_expired(ObjectId1, AppCtx),
          % this is NOT resetted
          limitless_backend:reset_expired(ObjectId3, AppCtx),
          % check db
          [#{<<"expiry">> := {_, Sec1After, _},
             <<"current">> := 0}] = limitless_backend:bulk_read(
                                      ObjectId1, AppCtx),
          [#{<<"expiry">> := {_, Sec3After, _},
             <<"current">> := 1}] = limitless_backend:bulk_read(
                                      ObjectId3, AppCtx),
          % check is resetted
          ?assertEqual(true, Sec1 < Sec1After),
          % check is NOT resetted
          ?assertEqual(Sec3, Sec3After)
        end
      ]
    end
  }.

next_id_test() ->
  AppCtx = #{backend => limitless_backend_mongopool, backendctx => bar},
  Ids = lists:map(fun(_) -> limitless_backend:next_id(AppCtx) end, lists:seq(1, 100)),
  ?assertEqual(
    erlang:length(Ids),
    erlang:length(sets:to_list(sets:from_list(Ids)))).

extra_info_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(AppCtx) -> [
        fun() ->
          {Id1, ObjectId1, Frequency1, MaxRequests1} = fixture_limit_1(),
          {Id3, ObjectId3, Frequency3, MaxRequests3} = fixture_limit_3(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Id1, ObjectId1, Frequency1, MaxRequests1, AppCtx),
          {ok, _} = limitless_backend:create(
                    Id3, ObjectId3, Frequency3, MaxRequests3, AppCtx),
          timer:sleep(1000),
          lists:foreach(fun(Index) ->
              % increment counters
              limitless_backend:inc(ObjectId1, AppCtx),
              check_info(ObjectId1, Frequency1, Index, AppCtx),
              check_info(ObjectId3, Frequency3, MaxRequests3, AppCtx)
            end, lists:seq(MaxRequests1 - 1, 0, -1)),
          limitless_backend:inc(ObjectId1, AppCtx),
          {true, _} = limitless_backend:is_reached(ObjectId1, AppCtx),
          {false, _} = limitless_backend:is_reached(ObjectId3, AppCtx),
          ok
        end
      ]
    end
  }.

%% Private functions
check_info(ObjectId, Frequency, Count, AppCtx) ->
  % check is reached
  {false, Limits} = limitless_backend:is_reached(ObjectId, AppCtx),
  % and looks inside extra info
  [{false, Count, Frequency1Info}] = limitless_backend:extra_info(Limits),
  ?assertEqual(true, Frequency1Info < Frequency).
