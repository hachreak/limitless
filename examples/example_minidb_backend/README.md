example_minidb_backend
=======================

An example of a web application with an integration with the rate-limiter
limitless.

Run the demo
------------

In the demo we are simulating a webserver with a `rate-limiter` who is
protecting the pages by checking the token passed by the request.

When we initialize the application, inside the
`example_minidb_backend_app.erl`, we create the tokens `token1` and `token2`.

Let's try the demo with the commands:

    $ rebar3 compile
    $ make clean
    $ make run

Now, try if the webserver works by opening a new shell and execute:

    $ curl http://127.0.0.1:8080/?token=token1 --verbose

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

If you try to use any other token different from the tokens configured
(in the demo, only `token1` and `token2`), e.g. `token3`, then you'll
receive a `401 Unauthorized` error:

```
  $ curl http://127.0.0.1:8080/?token=token3 --verbose
```

You will see a response like:

```
> GET /?token=token3 HTTP/1.1
> Host: 127.0.0.1:8080
> User-Agent: curl/7.52.1
> Accept: */*
>
< HTTP/1.1 401 Unauthorized
< server: Cowboy
< date: Mon, 01 May 2017 10:07:15 GMT
< content-length: 0
< www-authenticate: token="wrong"
```
