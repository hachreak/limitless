example_mongodb_backend
=======================

An example of a web application with an integration with the rate-limiter
limitless.

Run the demo
------------

```
  $ docker-compose build
  $ docker-compose up
```

In another console, initialize the demo with the commands:

```
  $ docker exec -it examplemongodbbackend_mongo_1 mongo test \
      --eval "db.dropDatabase()"
  $ docker exec -it examplemongodbbackend_web_1 bash
  $web> rebar3 shell --config priv/example_limitless.config
    1> {ok, Ctx} = limitless:init().
    2> limitless:setup(<<"token1">>, token, Ctx).
    3> limitless:setup(<<"token2">>, token, Ctx).
    4> application:ensure_all_started(example_mongodb_backend).
```

Open a new shell and execute:

```
  $ curl http://127.0.0.1:8080/?token=token1 --verbose
```

You will see a response like:

```
> GET /?token=token1 HTTP/1.1
> Host: 127.0.0.1:8080
> User-Agent: curl/7.52.1
> Accept: */*
>
< HTTP/1.1 200 OK
< server: Cowboy
< date: Thu, 20 Apr 2017 21:06:29 GMT
< content-length: 128
< X-RateLimit-Token-Daily-Limit: 1000
< X-RateLimit-Token-Daily-Remaining: 1000
< X-RateLimit-Token-Daily-Reset: 86250
< X-RateLimit-Token-15min-Limit: 100
< X-RateLimit-Token-15min-Remaining: 100
< X-RateLimit-Token-15min-Reset: 750
< content-type: text/html
<
<html>
<head>
        <meta charset="utf-8">
        <title>REST Hello World!</title>
</head>
<body>
        <p>REST Hello World!</p>
</body>
```
