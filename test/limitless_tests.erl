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
    ]).

app_start() ->
  mongo_start(),
  application:set_env(
    limitless, backend, [
      {name, limitless_backend_mongopool},
      {config, [
        {table, limitless},
        {pool, eshpool}
      ]}
    ]),
  application:set_env(
    limitless, limits, [
      {user, [
        [
          {type, <<"User-Daily">>},
          {frequency, 86400},
          {requests, 5000}
        ],
        [
          {type, <<"User-15min">>},
          {frequency, 54000},
          {requests, 1000}
        ]
      ]}
    ]
   ),
  {ok, AppCtx} = limitless:init(),
  AppCtx.

mongo_stop(#{ctx := Ctx}) ->
  application:unset_env(mongopool, pools),
  limitless_backend:drop(Ctx).

app_stop(AppCtx) ->
  mongo_stop(AppCtx),
  application:unset_env(limitless, backend),
  application:unset_env(limitless, limits).

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
  ObjectId = <<"obj-4">>,
  Frequency = 1,
  MaxRequests = 10,
  {Type, Id, ObjectId, Frequency, MaxRequests}.

setup_test_() ->
  {setup,
    fun app_start/0,
    fun app_stop/1,
    fun(#{ctx := Ctx}=AppCtx) -> [
        fun() ->
          ObjectId = <<"pippo">>,
          Group = user,
          % check db empty
          [] = limitless_backend:bulk_read(ObjectId, Ctx),
          % setup limits
          limitless:setup(ObjectId, Group, AppCtx),
          % check
          [Limit1, Limit2] = limitless_backend:bulk_read(ObjectId, Ctx),
          check_limit(Limit1, <<"User-Daily">>, ObjectId, 86400, 5000),
          check_limit(Limit2, <<"User-15min">>, ObjectId, 54000, 1000)
        end
      ]
    end
  }.

is_reached_test_() ->
  {setup,
    fun app_start/0,
    fun app_stop/1,
    fun(#{ctx := Ctx}=AppCtx) -> [
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
          % reach the limit for Id1
          timer:sleep(1000),
          lists:foreach(fun(Index) ->
              Req1 = MaxRequests1 - Index,
              Req2 = MaxRequests2 - Index,
              {false, ObjectId1, [
                {false, ObjectId1, [
                  {Type1, MaxRequests1, Req1, Frequency1Info},
                  {Type2, MaxRequests2, Req2, Frequency2Info}
                ]}
              ]} = limitless:is_reached([ObjectId1], AppCtx),
              ?assertEqual(true, Frequency1Info < Freq1),
              ?assertEqual(true, Frequency2Info < Freq2)
            end, lists:seq(0, MaxRequests1 - 1)),
          Req2 = MaxRequests2 - MaxRequests1,
          % check remaining is always 0 and is_react is true
          lists:foreach(fun(_) ->
              ?assertMatch({true, notfound, [
                  {true, ObjectId1, [
                    {Type1, MaxRequests1, 0, _},
                    {Type2, MaxRequests2, Req2, _}
                  ]}
                ]}, limitless:is_reached([ObjectId1], AppCtx))
            end, lists:seq(1, 10)),
          % check limit3 is not reached
          {false, ObjectId3, [
            {false, ObjectId3,
              [{Type3, MaxRequests3, MaxRequests3, Freq3Info}]}
           ]} = limitless:is_reached([ObjectId3], AppCtx),
          ?assertEqual(true, Freq3Info < Freq3),
          % check limit of a objectid that doesn't exists
          ?assertMatch(
            {false, <<"doesnt-exist">>,
              [{false, <<"doesnt-exist">>, []}]
            }, limitless:is_reached([<<"doesnt-exist">>], AppCtx))
        end
      ]
    end
  }.

is_reached_multiple_test_() ->
  {setup,
    fun app_start/0,
    fun app_stop/1,
    fun(#{ctx := Ctx}=AppCtx) -> [
        fun() ->
          {Type1, Id1, ObjectId1, Freq1, MaxRequests1} = fixture_limit_1(),
          {Type2, Id2, ObjectId1, Freq2, MaxRequests2} = fixture_limit_2(),
          {Type3, Id3, ObjectId3, Freq3, MaxRequests3} = fixture_limit_3(),
          {Type4, Id4, ObjectId4,
           Freq4, MaxRequests4} = fixture_expired_limit_4(),
          % create limits
          {ok, _} = limitless_backend:create(
                    Type1, Id1, ObjectId1, Freq1, MaxRequests1, Ctx),
          {ok, _} = limitless_backend:create(
                    Type2, Id2, ObjectId1, Freq2, MaxRequests2, Ctx),
          {ok, _} = limitless_backend:create(
                    Type3, Id3, ObjectId3, Freq3, MaxRequests3, Ctx),
          {ok, _} = limitless_backend:create(
                    Type4, Id4, ObjectId4, Freq4, MaxRequests4, Ctx),
          % reach the limit for Id1
          timer:sleep(1500),
          lists:foreach(fun(Index) ->
              Req1 = MaxRequests1 - Index,
              Req2 = MaxRequests2 - Index,
              {false, ObjectId1, [
                {false, ObjectId1, [
                  {Type1, MaxRequests1, Req1, Frequency1Info},
                  {Type2, MaxRequests2, Req2, Frequency2Info}
                ]},
                {false, ObjectId3, [
                  {Type3, MaxRequests3, MaxRequests3, Frequency3Info}
                ]},
                {false, ObjectId4, [
                  {Type4, MaxRequests4, MaxRequests4, Frequency4Info}
                ]}
              ]} = limitless:is_reached(
                     [ObjectId1, ObjectId3, ObjectId4], AppCtx),
              ?assertEqual(true, Frequency1Info < Freq1),
              ?assertEqual(true, Frequency2Info < Freq2),
              ?assertEqual(true, Frequency3Info < Freq3),
              ?assertEqual(true, Frequency4Info =< Freq4)
            end, lists:seq(0, MaxRequests1 - 1)),
          Req2 = MaxRequests2 - MaxRequests1,
          % check remaining is always 0 and is_react is true
          lists:foreach(fun(Index) ->
              Req3 = MaxRequests3 - Index,
              ?assertMatch({false, ObjectId3, [
                  {true, ObjectId1, [
                    {Type1, MaxRequests1, 0, _},
                    {Type2, MaxRequests2, Req2, _}
                  ]},
                  {false, ObjectId3, [
                    {Type3, MaxRequests3, Req3, _}
                  ]}
                ]}, limitless:is_reached([ObjectId1, ObjectId3], AppCtx))
            end, lists:seq(0, MaxRequests3 - 1)),
          % check limit3 is not reached
          {true, notfound, [
            {true, ObjectId1, [
              {Type1, MaxRequests1, 0, _},
              {Type2, MaxRequests2, _, _}
            ]},
            {true, ObjectId3,
              [{Type3, MaxRequests3, 0, Freq3Info}]}
           ]} = limitless:is_reached([ObjectId1, ObjectId3], AppCtx),
          ?assertEqual(true, Freq3Info < Freq3),
          % check limit of a objectid that doesn't exists
          ?assertMatch(
            {false, <<"doesnt-exist">>,
              [{false, <<"doesnt-exist">>, []}]
            }, limitless:is_reached([<<"doesnt-exist">>], AppCtx))
        end
      ]
    end
  }.

next_id_test() ->
  BackendCtx = #{backend => limitless_backend_mongopool, backendctx => bar},
  AppCtx = #{ctx => BackendCtx, limits => mylist},
  Ids = lists:map(fun(_) ->
      {ok, Id} = limitless:next_id(AppCtx),
      Id
    end, lists:seq(1, 100)),
  ?assertEqual(
    erlang:length(Ids),
    erlang:length(sets:to_list(sets:from_list(Ids)))).

%% Private functions

check_limit(Limit, Type, ObjectId, Frequency, MaxRequests) ->
  ?assertEqual(Type, maps:get(<<"type">>, Limit)),
  ?assertEqual(true, maps:is_key(<<"_id">>, Limit)),
  ?assertEqual(ObjectId, maps:get(<<"objectid">>, Limit)),
  ?assertEqual(Frequency, maps:get(<<"frequency">>, Limit)),
  ?assertEqual(MaxRequests, maps:get(<<"max">>, Limit)),
  ?assertEqual(MaxRequests, maps:get(<<"current">>, Limit)),
  ?assertEqual(true, maps:is_key(<<"expiry">>, Limit)).

init_without_config_limits_test_() ->
  {setup,
    fun() ->
      application:set_env(
        limitless, backend, [
          {name, limitless_backend_mongopool},
          {config, [
            {table, limitless},
            {pool, eshpool}
          ]}
        ])
    end,
    fun(_) ->
      application:unset_env(limitless_backend, backend)
    end,
    fun(_) -> [
        fun() ->
            {ok, Init} = limitless:init(),
            ?assertEqual(true, not maps:is_key(limits, Init))
        end
      ]
    end
  }.

