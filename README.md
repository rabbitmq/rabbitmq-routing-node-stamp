# RabbitMQ Routing Node Stamp Plugin

When a RabbitMQ broker receives a message, stamp the message with the node that routed it (handled client request).

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installing ##

Install the corresponding .ez files from our
[Community Plugins page](http://www.rabbitmq.com/community-plugins.html).


### Building from Source

Clone the repo and then build it with `make`:

```
cd rabbitmq-routing-node-stamp
make
# [snip]
make dist
# [snip]
ls plugins/*
```

Build artefacts then can be found under the `plugins` directory.

Finally copy `plugins/rabbitmq_routing_node_stamp_plugins.ez` to the `$RABBITMQ_HOME/plugins` folder.

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_routing_node_stamp
```

The plugin will then hook into the `basic.publish` process in order to inject a header containing the name of the routing node.

## Limitations ##

The plugin hooks into the `basic.publish` path, so expect a small
throughput reduction when using this plugin, since it has to modify
every message that crosses RabbitMQ.

This plugin should not be enabled at the same time as any other interceptors that hook into the `basic.publish` process, such as the `rabbitmq-message-timestamp` plugin. Enabling more than one interceptor that is registered to the `basic.publish` process will cause all AMQP 0-9-1 connections to fail when creating a new channel.

## Copyright and License ##

(c) Pivotal Software Inc, 2007-2016.

See the LICENSE file.
