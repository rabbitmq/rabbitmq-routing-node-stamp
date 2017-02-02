PROJECT = rabbitmq_routing_node_stamp
PROJECT_DESCRIPTION = Stamps a message with the name of the RabbitMQ node that accepted it from the client

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, ["3.6.0", "3.7.0"]}
endef

DEPS = rabbit_common rabbit
TEST_DEPS += ct_helper rabbitmq_ct_helpers rabbitmq_ct_client_helpers amqp_client
dep_ct_helper = git https://github.com/extend/ct_helper.git master

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
