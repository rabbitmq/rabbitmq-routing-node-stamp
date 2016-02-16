%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Message Timestamp.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_routing_node_test).

-export([test/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("rabbit_routing_node.hrl").

-import(rabbit_basic, [header/2, prepend_table_header/3]).

-define(IRRELEVANT_HEADER, <<"x-irrelevant-header">>).
-define(IRRELEVANT_KEY, <<"irrelevant-key">>).
-define(IRRELEVANT_VAL, <<"irrelevant-value">>).

test() ->
    ok = eunit:test(tests(?MODULE, 60), [verbose]).

% Verify that the message's routing node is added as a header.
routing_node_test() ->
    {ok, Conn} = amqp_connection:start(#amqp_params_network{}),
    {ok, Chan} = amqp_connection:open_channel(Conn),

    Ex = <<"e1">>,
    Q = <<"q">>,

    setup_fabric(Chan, make_exchange(Ex, <<"direct">>), make_queue(Q)),

    Msgs = [1],

    amqp_channel:call(Chan, #'confirm.select'{}),
    publish_messages(Chan, Ex, Msgs),
    amqp_channel:wait_for_confirms_or_die(Chan),

    {ok, Result} = consume(Chan, Q, Msgs, 5000),

    [begin
       ?assertNotEqual(get_headers(Msg), undefined),
       ?assert(header_found(?ROUTING_NODE_HEADER, Msg)),
       ?assertEqual(routed_by(Msg), node())
     end|| Msg <- Result],

    amqp_channel:call(Chan, delete_queue(Q)),
    amqp_channel:call(Chan, delete_exchange(Ex)),

    ok.

% Verify that an existing 'routed-by' header is not overwritten.
existing_routing_node_test() ->
    {ok, Conn} = amqp_connection:start(#amqp_params_network{}),
    {ok, Chan} = amqp_connection:open_channel(Conn),

    Ex = <<"e1">>,
    Q = <<"q">>,

    setup_fabric(Chan, make_exchange(Ex, <<"direct">>), make_queue(Q)),

    Msgs = [1],
    Nodes = lists:duplicate(1, seti@home),

    amqp_channel:call(Chan, #'confirm.select'{}),
    publish_accepted_messages(Chan, Ex, Msgs, Nodes),
    amqp_channel:wait_for_confirms_or_die(Chan),

    {ok, Result} = consume(Chan, Q, Msgs, 5000),

    [begin
       ?assertNotEqual(get_headers(Msg), undefined),
       ?assert(header_found(?ROUTING_NODE_HEADER, Msg)),
       ?assertEqual(routed_by(Msg), seti@home)
     end|| Msg <- Result],

    amqp_channel:call(Chan, delete_queue(Q)),
    amqp_channel:call(Chan, delete_exchange(Ex)),

    ok.

% Verify that the 'routed-by' operation is orthogonal to any 
% othe pre-existing headers.
routing_node_irrelevant_header_test() ->
    {ok, Conn} = amqp_connection:start(#amqp_params_network{}),
    {ok, Chan} = amqp_connection:open_channel(Conn),

    Ex = <<"e1">>,
    Q = <<"q">>,

    setup_fabric(Chan, make_exchange(Ex, <<"direct">>), make_queue(Q)),

    Msgs = [1],

    amqp_channel:call(Chan, #'confirm.select'{}),
    publish_messages_with_irrelevant_headers(Chan, Ex, Msgs),
    amqp_channel:wait_for_confirms_or_die(Chan),

    {ok, Result} = consume(Chan, Q, Msgs, 5000),

    [begin
       ?assertNotEqual(get_headers(Msg), undefined),
       ?assert(header_found(?ROUTING_NODE_HEADER, Msg)),
       ?assert(header_found(?IRRELEVANT_HEADER, Msg)),
       ?assertEqual(routed_by(Msg), node())
     end|| Msg <- Result],

    amqp_channel:call(Chan, delete_queue(Q)),
    amqp_channel:call(Chan, delete_exchange(Ex)),

    ok.


% Test framework setup

setup_fabric(Chan, ExDeclare, QueueDeclare) ->
    setup_fabric(Chan, ExDeclare, QueueDeclare, <<>>).

setup_fabric(Chan,
             ExDeclare = #'exchange.declare'{exchange = Ex},
             QueueDeclare,
             RK) ->
    declare_exchange(Chan, ExDeclare),

    #'queue.declare_ok'{queue = Q} =
        amqp_channel:call(Chan, QueueDeclare),

    #'queue.bind_ok'{} =
        amqp_channel:call(Chan, #'queue.bind' {
                                   queue       = Q,
                                   exchange    = Ex,
                                   routing_key = RK
                                  }).

declare_exchange(Chan, ExDeclare) ->
    #'exchange.declare_ok'{} =
        amqp_channel:call(Chan, ExDeclare).

% MsgGenerator is a function that produces a single #amqp_msg from the inputs.
% Msgs is a list of inputs used by the MsgGenerator.
publish(Chan, Ex, RK, MsgGenerator, [_|_] = MsgInputs) ->
    [amqp_channel:call(Chan,
                       #'basic.publish'{exchange = Ex, routing_key = RK},
                       MsgGenerator(V)) || V <- MsgInputs].

consume(Chan, Q, Msgs, Timeout) ->
    #'basic.consume_ok'{} =
        amqp_channel:subscribe(Chan, #'basic.consume'{queue  = Q,
                                                      no_ack = true}, self()),
    collect(length(Msgs), Timeout).


collect(N, Timeout) ->
    collect(0, N, Timeout, []).

collect(N, N, _Timeout, Acc) ->
    {ok, lists:reverse(Acc)};
collect(Curr, N, Timeout, Acc) ->
    receive {#'basic.deliver'{},
             Msg = #amqp_msg{}} ->
            collect(Curr+1, N, Timeout, [Msg | Acc])
    after Timeout ->
            {error, {timeout, Acc}}
    end.

delete_exchange(Ex) ->
    #'exchange.delete' {
       exchange       = Ex
      }.

delete_queue(Q) ->
    #'queue.delete' {
       queue       = Q
      }.

make_queue(Q) ->
    #'queue.declare' {
       queue       = Q
      }.

make_exchange(Ex, Type) ->
    #'exchange.declare'{
       exchange    = Ex,
       type        = Type
      }.

%%% Test-Specific Helpers

publish_messages(Chan, Ex, Msgs) ->
    publish(Chan, Ex, <<>>, fun make_msg/1, Msgs).

publish_accepted_messages(Chan, Ex, Msgs, Nodes) ->
  publish(Chan, Ex, <<>>, fun make_routed_msg/1, lists:zip(Msgs, Nodes)).

publish_messages_with_irrelevant_headers(Chan, Ex, Msgs) ->
  publish(Chan, Ex, <<>>, fun make_msg_with_irrelevant_header/1, Msgs).


get_headers(#amqp_msg{props = #'P_basic'{headers = Headers}}) -> Headers.

%TODO: Find a representation, based on the semantics of the AMQP header.
routed_by(#amqp_msg{props = #'P_basic'{headers = Headers}}) ->
  list_to_atom(binary_to_list(element(3,hd(element(2,hd(element(3, header(?ROUTING_NODE_HEADER, Headers)))))))).


make_msg(V) -> #amqp_msg{payload = term_to_binary(V)}.

make_routed_msg({V,N}) ->
    make_msg_with_header(V,
                         ?ROUTING_NODE_HEADER,
                         ?ROUTING_NODE_KEY, 
                         list_to_binary(atom_to_list(N))).

make_msg_with_irrelevant_header(V) ->
    make_msg_with_header(V, ?IRRELEVANT_HEADER, ?IRRELEVANT_KEY, ?IRRELEVANT_VAL).

header_found(<<_,_/binary>> = Target, #amqp_msg{props = #'P_basic'{headers = Headers}}) ->
  lists:keyfind(Target, 1, Headers) /= false.

make_msg_with_header(V,Name,Key,Value) ->
    NewHeaders = prepend_table_header(
                  Name,
                  [{Key, longstr, Value}],
                  undefined),
    #amqp_msg{payload = term_to_binary(V), props = #'P_basic'{headers = NewHeaders}}.


tests(Module, Timeout) ->
  {foreach, fun() -> ok end,
   [{timeout, Timeout, fun () -> Module:F() end} ||
    {F, _Arity} <- proplists:get_value(exports, Module:module_info()),
    string:right(atom_to_list(F), 5) =:= "_test"]}.
