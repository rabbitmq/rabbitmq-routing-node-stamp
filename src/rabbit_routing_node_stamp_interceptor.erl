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
%% The Original Code is RabbitMQ Routing Node.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_routing_node_stamp_interceptor).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("rabbit_routing_node_stamp.hrl").

-import(rabbit_basic, [extract_headers/1, header/2, prepend_table_header/3]).

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1]).

-rabbit_boot_step({?MODULE,
                   [{description, "routing node stamp interceptor"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"routing node stamp interceptor">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"routing node stamp interceptor">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

init(_Ch) ->
  undefined.

description() ->
  [{description,
    <<"Adds name of receiving node to messages as they enter RabbitMQ">>}].

intercept(#'basic.publish'{} = Method, Content, _IState) ->
  DecodedContent = rabbit_binary_parser:ensure_content_decoded(Content),
  Content2 = set_routing_node(DecodedContent),
  {Method, Content2};

intercept(Method, Content, _VHost) ->
  {Method, Content}.

applies_to() ->
  ['basic.publish'].

%%----------------------------------------------------------------------------

set_routing_node(#content{properties = #'P_basic'{headers = Headers}} = Content) ->
  case header(?ROUTING_NODE_HEADER, Headers) of
    undefined -> set_routing_node(Content, node());
    _ -> Content  % Do not overwrite an existing routing node.
  end.

set_routing_node(#content{properties = #'P_basic'{headers = Headers} = Props} = Content, Node) ->
  NewHeaders = add_header(Headers, new_routing_header(Node)),
  Content#content{
    properties = Props#'P_basic'{headers = NewHeaders},
    properties_bin = none
   }.

add_header(undefined, Header) -> [Header];
add_header(Headers, Header) ->
  lists:keystore(element(1, Header), 1, Headers, Header).

new_routing_header(Node) ->
  {?ROUTING_NODE_HEADER, longstr, atom_to_binary(Node, utf8)}.
