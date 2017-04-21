limitless
=========

[![Build Status](https://travis-ci.org/hachreak/limitless.svg?branch=master)](https://travis-ci.org/hachreak/limitless)

Limitless is a lightweight, generic library for Erlang to quickly integrate a
rate-limiter inside your application.

Example
-------

See `examples` directory to see how use the library with the `mongodb` backend.

Configuration
-------------

Add `limitless` to your `rebar.config` deps:

```erlang
{deps,
 [
  {limitless, "",
   {git, "https://github.com/rpt/limitless.git",
    {tag, "v1.0.0"}}}
 ]}.
```

Remember to add also the libraries needed by the specific backend (in the
mongodb example: `uuid`, `mongopool`).

Add a configuration for specify which backend are you using and which kind
of limits are you imposing.

In this example we define that every request of type `token` will be controlled
by two different limits:

 1. The first limit is 1000 req/day
 2. The other limit is 100 req/15min

```erlang
[
  {limitless, [
    {backend, [
      % specify the backend to use
      {name, limitless_backend_mongopool},
      % and the backend configuration
      {config, [
        {table, limitless},
        {pool, mymongopool}
      ]}
    ]},
    {limits, [
      {token, [
        [
          % max 1000 req/day
          {type, <<"Token-Daily">>},
          {frequency, 86400}, % 1 day = 3600 * 24h
          {requests, 1000}
        ],
        [
          % max 100 req/15min
          {type, <<"Token-15min">>},
          {frequency, 900}, % 15 min = 60 * 15
          {requests, 100}
        ]
      ]}
    ]}
  ]}
]
```

Note: in the configuration there is also a term `mongopool` to complete the
backend configuration.

On the application running, when a new object `token` we'll be created, also
should be registered in the limits lists with:

```erlang
{ok, Ctx} = limitless:init().
limitless:setup(RequestToken, token, ctx).
```

In this way, every time you receive a request from a token `RequestToken`,
you be able to check if it reached any of its limits
(in the example: 1000 req/day and 100 req/15min):

```erlang
{Result, ConsumedTokens, InfoTokens} = limitless:is_reached(
                                                [RequestToken], LimitlessCtx)
```

If `Result` is `true`, it means that at least one limit is reached
(see `ConsumedTokens` to know which one).

The `InfoTokens` are useful information to build a table about the current
situation.

In the example, it's used to build the HTTP relative headers:

```erlang
Req2 = limitless_cowboy_utils:set_rate_limiter_headers(InfoTokens, Req),
```

It'll add the following headers to the `cowboy` response:

```
X-RateLimit-Token-Daily-Limit: 1000
X-RateLimit-Token-Daily-Remaining: 1000
X-RateLimit-Token-Daily-Reset: 86250
X-RateLimit-Token-15min-Limit: 100
X-RateLimit-Token-15min-Remaining: 100
X-RateLimit-Token-15min-Reset: 750
```

Status
------

Stable release.
