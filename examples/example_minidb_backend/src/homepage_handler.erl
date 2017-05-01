%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(homepage_handler).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
% -export([malformed_request/2]).
-export([is_authorized/2]).
-export([hello_to_html/2]).

init(_Transport, _Req, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, AppCtx) ->
    {ok, Req, AppCtx}.

is_authorized(Req, {ValidTokens, LimitlessCtx}) ->
  {RequestToken, Req2} = cowboy_req:qs_val(<<"token">>, Req),
  case lists:member(RequestToken, ValidTokens) of
    true ->
      % check if there is some limit reached (req/day or req/15min)
      {Result, _ConsumedTokens, InfoTokens} = limitless:is_reached(
                                                [RequestToken], LimitlessCtx),
      % set cowboy headers
      Req3 = limitless_cowboy_utils:set_rate_limiter_headers(InfoTokens, Req2),
      % forbidden if a limit is reached, otherwise return the page
      case Result of
        false -> {true, Req3, LimitlessCtx};
        true -> {{false, <<"token=\"consumed\"">>}, Req3, LimitlessCtx}
      end;
    _ -> {{false, <<"token=\"wrong\"">>}, Req2, LimitlessCtx}
  end.

content_types_provided(Req, AppCtx) ->
	{[{<<"text/html">>, hello_to_html}], Req, AppCtx}.

hello_to_html(Req, AppCtx) ->
	Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Hello World!</title>
</head>
<body>
	<p>REST Hello World!</p>
</body>
</html>">>,
	{Body, Req, AppCtx}.
