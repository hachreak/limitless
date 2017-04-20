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
%%% @doc limitless utils - tests.
%%% @end

-module(limitless_cowboy_utils_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

rate_limiter_headers_test() ->
  Headers = limitless_cowboy_utils:rate_limiter_headers([
      {false,<<"zap">>, [
        {<<"limit1">>, 30, 20, 100},
        {<<"limit2">>, 70, 80, 200}
      ]},
      {false,<<"fuu">>, [
        {<<"limit3">>, 40, 30, 400},
        {<<"limit4">>, 50, 10, 500}
      ]}
    ]),
  ExpectedHeaders = [
      {<<"X-RateLimit-limit1-Limit">>, <<"30">>},
      {<<"X-RateLimit-limit1-Remaining">>, <<"20">>},
      {<<"X-RateLimit-limit1-Reset">>, <<"100">>},
      {<<"X-RateLimit-limit2-Limit">>, <<"70">>},
      {<<"X-RateLimit-limit2-Remaining">>, <<"80">>},
      {<<"X-RateLimit-limit2-Reset">>, <<"200">>},
      {<<"X-RateLimit-limit3-Limit">>, <<"40">>},
      {<<"X-RateLimit-limit3-Remaining">>, <<"30">>},
      {<<"X-RateLimit-limit3-Reset">>, <<"400">>},
      {<<"X-RateLimit-limit4-Limit">>, <<"50">>},
      {<<"X-RateLimit-limit4-Remaining">>, <<"10">>},
      {<<"X-RateLimit-limit4-Reset">>, <<"500">>}
    ],

  ?assertEqual(ExpectedHeaders, Headers).
