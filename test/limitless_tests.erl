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

is_reached_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(AppCtx) -> [
        fun() ->
          {Type1, Id1, ObjectId1, Freq1, MaxRequests1} = fixture_limit_1(),
          {Type2, Id2, ObjectId1, Freq2, MaxRequests2} = fixture_limit_2(),
          {Type3, Id3, ObjectId3, Freq3, MaxRequests3} = fixture_limit_3(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Type1, Id1, ObjectId1, Freq1, MaxRequests1, AppCtx),
          {ok, _} = limitless_backend:create(
                    Type2, Id2, ObjectId1, Freq2, MaxRequests2, AppCtx),
          {ok, _} = limitless_backend:create(
                    Type3, Id3, ObjectId3, Freq3, MaxRequests3, AppCtx),
          % reach the limit for Id1
          % limitless:is_reached(ObjectId1, AppCtx),
          timer:sleep(1000),
          lists:foreach(fun(Index) ->
              Req1 = MaxRequests1 - Index,
              Req2 = MaxRequests2 - Index,
              {false, [{Type1, MaxRequests1, Req1, Frequency1Info},
                       {Type2, MaxRequests2, Req2, Frequency2Info}
                      ]} = limitless:is_reached(ObjectId1, AppCtx),
              ?assertEqual(true, Frequency1Info < Freq1),
              ?assertEqual(true, Frequency2Info < Freq2)
            end, lists:seq(1, MaxRequests1)),
          Req2 = MaxRequests2 - MaxRequests1 - 1,
          {true, [{Type1, MaxRequests1, -1, _},
                  {Type2, MaxRequests2, Req2, _}
                 ]} = limitless:is_reached(ObjectId1, AppCtx),
          Req3 = MaxRequests3 - 1,
          {false,
           [{Type3, MaxRequests3,
             Req3, Freq3Info}]} = limitless:is_reached(ObjectId3, AppCtx),
          ?assertEqual(true, Freq3Info < Freq3),
          % check limit of a objectid that doesn't exists
          ?assertMatch({false, []}, limitless:is_reached(
                                     <<"doesnt-exist">>, AppCtx))
        end
      ]
    end
  }.

next_id_test() ->
  AppCtx = #{backend => limitless_backend_mongopool, backendctx => bar},
  Ids = lists:map(fun(_) ->
      {ok, Id} = limitless:next_id(AppCtx),
      Id
    end, lists:seq(1, 100)),
  ?assertEqual(
    erlang:length(Ids),
    erlang:length(sets:to_list(sets:from_list(Ids)))).

create_test_() ->
  {setup,
    fun mongo_start/0,
    fun mongo_stop/1,
    fun(AppCtx) -> [
        fun() ->
          {Type, Id, ObjectId, Frequency, MaxRequests} = fixture_limit_1(),
          % check db empty
          [] = limitless_backend:bulk_read(ObjectId, AppCtx),
          % create limit
          {ok, Limit} = limitless:create(
                    Type, Id, ObjectId, Frequency, MaxRequests, AppCtx),
          % check returned value
          ?assertEqual(Type, maps:get(<<"type">>, Limit)),
          ?assertEqual(Id, maps:get(<<"_id">>, Limit)),
          ?assertEqual(ObjectId, maps:get(<<"objectid">>, Limit)),
          ?assertEqual(Frequency, maps:get(<<"frequency">>, Limit)),
          ?assertEqual(MaxRequests, maps:get(<<"max">>, Limit)),
          ?assertEqual(0, maps:get(<<"current">>, Limit)),
          ?assertEqual(true, maps:is_key(<<"expiry">>, Limit)),
          % check database
          [LimitWrote] = limitless_backend:bulk_read(ObjectId, AppCtx),
          ?assertEqual(Type, maps:get(<<"type">>, LimitWrote)),
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
