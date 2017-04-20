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
%%% @doc limitless cowboy utils
%%% @end

-module(limitless_cowboy_utils).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([set_rate_limiter_headers/2]).

-type limits_extra() :: limitless:limits_extra().
-type limits_info()  :: limitless_backend:limits_info().
-type req()          :: cowboy_req:req().


% Set rate-limiter headers
-spec set_rate_limiter_headers(limits_extra(), req()) -> req().
set_rate_limiter_headers(LimitsInfo, Req) ->
  set_headers(rate_limiter_headers(LimitsInfo), Req).

%% Private functions

-spec set_headers({binary(), binary()}, req()) -> req().
set_headers(Headers, Req) ->
  lists:foldl(
    fun({Header, Value}, ReqIn) ->
      ReqWithHeader = cowboy_req:set_resp_header(Header, Value, ReqIn),
      ReqWithHeader
    end, Req, Headers).

-spec rate_limiter_headers(limits_extra()) ->
  list({binary(), binary()}).
rate_limiter_headers(LimitsInfo) ->
  ListHeaders = lists:map(fun({_, _, LimitInfo}) ->
      rate_limiter_limit_headers(LimitInfo)
    end, LimitsInfo),
  lists:flatten(ListHeaders).

% Rate-Limter headers builder
-spec rate_limiter_limit_headers(limits_info()) ->
  list({binary(), binary()}).
rate_limiter_limit_headers(LimitInfo) ->
  ListHeaders = lists:map(fun({Type, Max, Remaining, WhenReset}) ->
      Base = << <<"X-RateLimit-">>/binary, Type/binary >>,
      [
       {<< Base/binary, <<"-Limit">>/binary >>,
        erlang:integer_to_binary(Max)},
       {<< Base/binary, <<"-Remaining">>/binary >>,
        erlang:integer_to_binary(Remaining)},
       {<< Base/binary, <<"-Reset">>/binary >>,
        erlang:integer_to_binary(WhenReset)}
      ]
    end, LimitInfo),
  lists:flatten(ListHeaders).

-ifdef(TEST).
-compile(export_all).
-endif.
