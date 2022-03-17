# RabbitMQ Routing Node Stamp Plugin

[![Build Status](https://github.com/rabbtitmq/rabbitmq-routing-node-stamp/actions/workflows/main.yaml/badge.svg?branch=master)](https://github.com/rabbitmq/rabbitmq-routing-node-stamp/actions)

When a RabbitMQ broker receives a message, stamp the message with the node that routed it (handled client request).

## Supported RabbitMQ and Erlang Versions ##

This plugin currently targets RabbitMQ 3.8.x and Erlang 21.3+. Please see the [Releases](https://github.com/rabbitmq/rabbitmq-routing-node-stamp/releases) page for binary downloads.

## Binary Builds

Binary builds are published as [GitHub releases](https://github.com/rabbitmq/rabbitmq-routing-node-stamp/releases).

See [Plugin Installation](https://www.rabbitmq.com/installing-plugins.html) for details
about how to install plugins that do not ship with RabbitMQ.

## Limitations

This plugin cannot be used together with [rabbitmq-message-timestamp](https://github.com/rabbitmq/rabbitmq-message-timestamp)
as they override the same extension point.

### Building from Source

Clone the repo and then build it with `make`:

```
cd rabbitmq-routing-node-stamp
make
# [snip]
make DIST_AS_EZS=true dist
# [snip]
ls plugins/*
```

Build artefacts then can be found under the `plugins` directory.

Finally copy `plugins/rabbitmq_routing_node_stamp_plugins-XXX.ez` to the `$RABBITMQ_HOME/plugins` folder.

## Usage ##

Enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_routing_node_stamp
```

The plugin will then hook into the `basic.publish` process in order to inject a header containing the name of the routing node.

## Limitations ##

The plugin hooks into the `basic.publish` path, so expect a small throughput reduction when using this plugin, since it has to modify every message that crosses RabbitMQ.

This plugin should not be enabled at the same time as any other interceptors that hook into the `basic.publish` process, such as the `rabbitmq-message-timestamp` plugin. Enabling more than one interceptor that is registered to the `basic.publish` process will cause all AMQP 0-9-1 connections to fail when creating a new channel.

## Copyright and License ##

(c) 2007-2022 VMware, Inc. or its affiliates.

See the LICENSE file.
