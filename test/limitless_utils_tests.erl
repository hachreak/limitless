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
%%% @doc limitless utils - tests.
%%% @end

-module(limitless_utils_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

get_or_fail_test() ->
  Value = limitless_utils:get_or_fail(a, [{b, c}, {a, d}]),
  ?assertEqual(d, Value),
  ?assertException(
     throw, not_found, limitless_utils:get_or_fail(
                         no_exists, [{b, c}, {a, d}])).
