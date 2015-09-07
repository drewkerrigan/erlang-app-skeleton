# erlang-app-skeleton

The goal of this project is to show how an extremely minimal Erlang application
might be written while using the following components:

* Webmachine [github.com/webmachine/webmachine](https://github.com/webmachine/webmachine)
* Clique [github.com/basho/clique](https://github.com/basho/clique)
* Node Package [github.com/basho/node_package](https://github.com/basho/node_package)

These valuable Erlang project dependencies are sometimes difficult to find
documentation for, so this simple `Sampler` application can serve as a resuable
example for myself and hopefully others.

## Sampler app

What does it do? It simply stores a status string in the state of a gen_server
and lets you interact with that state using either the command line or a web
endpoint.

### Building

```
make rel
```

### Testing

#### Run the sampler application

```
./rel/sampler/bin/sampler start
```

#### Clique Based CLI

Get the status

```
./rel/sampler/bin/sampler-admin status
```

```
{"status":"Initial Status"}
```

Set the status

```
./rel/sampler/bin/sampler-admin status -s Hello There
```

```
{"status":"Hello There"}
```

#### Webmachine HTTP Resource

Get the status

```
curl http://localhost:9000/sampler/status
```

```
{"status":"Hello There"}
```

Set the status

```
curl -i -XPUT -H "Content-Type: application/json" \
    'http://localhost:9000/sampler/status'
    -d '{"status": "Hi There"}'
```

```
HTTP/1.1 204 No Content
Server: MochiWeb/1.1 WebMachine/1.10.8 (that head fake, tho)
Date: Mon, 27 Jul 2015 20:12:17 GMT
Content-Type: application/json
Content-Length: 0
```
