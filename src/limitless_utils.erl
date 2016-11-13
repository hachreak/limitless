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
%%% @doc limitless utils.
%%% @end

-module(limitless_utils).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  get_or_fail/2,
  gregorian_second2timestamp/1,
  timestamp_to_gregorian_seconds/1
]).

%% Types

-type timestamp() :: erlang:timestamp().

%% API

-spec get_or_fail(any(), list()) -> any().
get_or_fail(Key, List) ->
  case proplists:get_value(Key, List) of
    undefined -> throw(not_found);
    Value -> Value
  end.

% @doc Computes the number of gregorian seconds starting with year 0 and
% ending at the specified timestamp.
% @end
-spec timestamp_to_gregorian_seconds(timestamp()) -> non_neg_integer().
timestamp_to_gregorian_seconds(Timestamp) ->
  calendar:datetime_to_gregorian_seconds(
    calendar:now_to_universal_time(Timestamp)).

% @doc Convert the number of gregorian seconds in a timestamp.
% @end
-spec gregorian_second2timestamp(non_neg_integer()) -> timestamp().
gregorian_second2timestamp(Seconds) ->
  FixSeconds = Seconds - 62167219200,
  {FixSeconds div 1000000, FixSeconds rem 1000000, 0}.
