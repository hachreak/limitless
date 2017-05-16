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
                   {host, os:getenv("DB_HOST", db)}
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
  {ok, BackendConfig} = application:get_env(limitless, backend),
  {ok, Ctx} = limitless_backend:init(BackendConfig),
  Ctx.

mongo_stop(Ctx) ->
  limitless_backend:drop(Ctx),
  application:unset_env(mongopool, pools),
  application:unset_env(limitless_backend, backend).

fixture_limit_1() ->
  Type = <<"montly">>,
  Id = <<"limit-1">>,
  ObjectId = <<"obj-1">>,
  Frequency = 3600,
  MaxRequests = 10,
  {Type, Id, ObjectId, Frequency, MaxRequests}.

fixture_limit_2() ->
  Type = <<"daily">>,
  Id = <<"limit-2">>,
  ObjectId = <<"obj-1">>,
  Frequency = 3600,
  MaxRequests = 20,
  {Type, Id, ObjectId, Frequency, MaxRequests}.

fixture_limit_3() ->
  Type = <<"daily">>,
  Id = <<"limit-3">>,
  ObjectId = <<"obj-2">>,
  Frequency = 3600,
  MaxRequests = 20,
  {Type, Id, ObjectId, Frequency, MaxRequests}.

fixture_expired_limit_4() ->
  Type = <<"daily">>,
  Id = <<"limit-4">>,
  ObjectId = <<"obj-1">>,
  Frequency = 1,
  MaxRequests = 10,
  {Type, Id, ObjectId, Frequency, MaxRequests}.

create_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(Ctx) -> [
        fun() ->
          {Type, Id, ObjectId, Frequency, MaxRequests} = fixture_limit_1(),
          % check db empty
          [] = limitless_backend:bulk_read(ObjectId, Ctx),
          % create limit
          {ok, Limit} = limitless_backend:create(
                    Type, Id, ObjectId, Frequency, MaxRequests, Ctx),
          % check returned value
          ?assertEqual(Type, maps:get(<<"type">>, Limit)),
          ?assertEqual(Id, maps:get(<<"_id">>, Limit)),
          ?assertEqual(ObjectId, maps:get(<<"objectid">>, Limit)),
          ?assertEqual(Frequency, maps:get(<<"frequency">>, Limit)),
          ?assertEqual(MaxRequests, maps:get(<<"max">>, Limit)),
          ?assertEqual(MaxRequests, maps:get(<<"current">>, Limit)),
          ?assertEqual(true, maps:is_key(<<"expiry">>, Limit)),
          % check database
          [LimitWrote] = limitless_backend:bulk_read(ObjectId, Ctx),
          ?assertEqual(Type, maps:get(<<"type">>, LimitWrote)),
          ?assertEqual(Id, maps:get(<<"_id">>, LimitWrote)),
          ?assertEqual(ObjectId, maps:get(<<"objectid">>, LimitWrote)),
          ?assertEqual(Frequency, maps:get(<<"frequency">>, LimitWrote)),
          ?assertEqual(MaxRequests, maps:get(<<"max">>, LimitWrote)),
          ?assertEqual(MaxRequests, maps:get(<<"current">>, LimitWrote)),
          ?assertEqual(true, maps:is_key(<<"expiry">>, LimitWrote))
        end
      ]
    end
  }.

delete_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(Ctx) -> [
        fun() ->
          {Type1, Id1, ObjectId, Frequency1, MaxRequests1} = fixture_limit_1(),
          {Type2, Id2, ObjectId, Frequency2, MaxRequests2} = fixture_limit_2(),
          % check db empty
          [] = limitless_backend:bulk_read(ObjectId, Ctx),
          % create limits
          {ok, _} = limitless_backend:create(
                    Type1, Id1, ObjectId, Frequency1, MaxRequests1, Ctx),
          {ok, _} = limitless_backend:create(
                    Type2, Id2, ObjectId, Frequency2, MaxRequests2, Ctx),
          % try to delete only the fixture 1
          limitless_backend:delete(Id1, Ctx),
          % check db
          [Limit] = limitless_backend:bulk_read(ObjectId, Ctx),
          ?assertEqual(Id2, maps:get(<<"_id">>, Limit))
        end
      ]
    end
  }.

dec_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(Ctx) -> [
        fun() ->
          {Type1, Id1, ObjectId1, Freq1, MaxRequests1} = fixture_limit_1(),
          {Type2, Id2, ObjectId1, Freq2, MaxRequests2} = fixture_limit_2(),
          {Type3, Id3, ObjectId3, Freq3, MaxRequests3} = fixture_limit_3(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Type1, Id1, ObjectId1, Freq1, MaxRequests1, Ctx),
          {ok, _} = limitless_backend:create(
                    Type2, Id2, ObjectId1, Freq2, MaxRequests2, Ctx),
          {ok, _} = limitless_backend:create(
                    Type3, Id3, ObjectId3, Freq3, MaxRequests3, Ctx),
          % check objectids
          ?assertNotEqual(ObjectId1, ObjectId3),
          % try to decrement current
          limitless_backend:dec(ObjectId1, Ctx),
          % check db
          [Limit1, Limit2] = limitless_backend:bulk_read(ObjectId1, Ctx),
          ?assertEqual(MaxRequests1 - 1, maps:get(<<"current">>, Limit1)),
          ?assertEqual(MaxRequests2 - 1, maps:get(<<"current">>, Limit2)),
          [Limit3] = limitless_backend:bulk_read(ObjectId3, Ctx),
          ?assertEqual(MaxRequests3, maps:get(<<"current">>, Limit3))
        end
      ]
    end
  }.

is_reached_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(Ctx) -> [
        fun() ->
          {Type1, Id1, ObjectId1, Freq1, MaxRequests1} = fixture_limit_1(),
          {Type2, Id2, ObjectId1, Freq2, MaxRequests2} = fixture_limit_2(),
          {Type3, Id3, ObjectId3, Freq3, MaxRequests3} = fixture_limit_3(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Type1, Id1, ObjectId1, Freq1, MaxRequests1, Ctx),
          {ok, _} = limitless_backend:create(
                    Type2, Id2, ObjectId1, Freq2, MaxRequests2, Ctx),
          {ok, _} = limitless_backend:create(
                    Type3, Id3, ObjectId3, Freq3, MaxRequests3, Ctx),
          % reset timers
          limitless_backend:reset_expired(ObjectId1, Ctx),
          limitless_backend:reset_expired(ObjectId3, Ctx),
          % check if is reached (false)
          ?assertMatch({false, _},
                       limitless_backend:is_reached(ObjectId1, Ctx)),
          ?assertMatch({false, _},
                       limitless_backend:is_reached(ObjectId3, Ctx)),
          % reach the limit for Id3
          lists:foreach(fun(_) ->
              limitless_backend:dec(ObjectId3, Ctx)
            end, lists:seq(1, MaxRequests3)),
          ?assertMatch({false, _},
                       limitless_backend:is_reached(ObjectId1, Ctx)),
          ?assertMatch({true, _},
                       limitless_backend:is_reached(ObjectId3, Ctx))
        end
      ]
    end
  }.

reset_expired_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(Ctx) -> [
        fun() ->
          {Type4, Id1, ObjectId1,
           Freq1, MaxRequests1} = fixture_expired_limit_4(),
          {Type3, Id3, ObjectId3, Freq3, MaxRequests3} = fixture_limit_3(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Type4, Id1, ObjectId1, Freq1, MaxRequests1, Ctx),
          {ok, _} = limitless_backend:create(
                    Type3, Id3, ObjectId3, Freq3, MaxRequests3, Ctx),
          % decrement counters
          limitless_backend:dec(ObjectId1, Ctx),
          limitless_backend:dec(ObjectId3, Ctx),
          % backup
          Current1 = MaxRequests1 - 1,
          Current3 = MaxRequests3 - 1,
          [#{<<"expiry">> := {_, Sec1, _},
             <<"current">> := Current1}] = limitless_backend:bulk_read(
                                      ObjectId1, Ctx),
          [#{<<"expiry">> := {_, Sec3, _},
             <<"current">> := Current3}] = limitless_backend:bulk_read(
                                      ObjectId3, Ctx),
          % reset timers after 3 seconds
          timer:sleep(3000),
          % this is resetted
          limitless_backend:reset_expired(ObjectId1, Ctx),
          % this is NOT resetted
          limitless_backend:reset_expired(ObjectId3, Ctx),
          % check db
          [#{<<"expiry">> := {_, Sec1After, _},
             <<"current">> := MaxRequests1}] = limitless_backend:bulk_read(
                                      ObjectId1, Ctx),
          NewCurrent3 = MaxRequests3 - 1,
          [#{<<"expiry">> := {_, Sec3After, _},
             <<"current">> := NewCurrent3}] = limitless_backend:bulk_read(
                                      ObjectId3, Ctx),
          % check is resetted
          ?assertEqual(true, Sec1 < Sec1After),
          % check is NOT resetted
          ?assertEqual(Sec3, Sec3After)
        end
      ]
    end
  }.

extra_info_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(Ctx) -> [
        fun() ->
          {Type1, Id1, ObjectId1, Freq1, MaxRequests1} = fixture_limit_1(),
          {Type3, Id3, ObjectId3, Freq3, MaxRequests3} = fixture_limit_3(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Type1, Id1, ObjectId1, Freq1, MaxRequests1, Ctx),
          {ok, _} = limitless_backend:create(
                    Type3, Id3, ObjectId3, Freq3, MaxRequests3, Ctx),
          timer:sleep(1000),
          lists:foreach(fun(Index) ->
              % decrement counters
              limitless_backend:dec(ObjectId1, Ctx),
              check_info(Type1, ObjectId1, Freq1, MaxRequests1, Index, Ctx),
              check_info(
                Type3, ObjectId3, Freq3, MaxRequests3, MaxRequests3, Ctx)
            end, lists:seq(MaxRequests1 - 1, 1, -1)),
          limitless_backend:dec(ObjectId1, Ctx),
          {true, _} = limitless_backend:is_reached(ObjectId1, Ctx),
          {false, _} = limitless_backend:is_reached(ObjectId3, Ctx),
          ok
        end
      ]
    end
  }.

%% Private functions
check_info(Type, ObjectId, Frequency, MaxRequests, Count, Ctx) ->
  % check is reached
  {false, Limits} = limitless_backend:is_reached(ObjectId, Ctx),
  % and looks inside extra info
  [{Type, MaxRequests,
    Count, Frequency1Info}] = limitless_backend:extra_info(Limits),
  ?assertEqual(true, Frequency1Info < Frequency).
