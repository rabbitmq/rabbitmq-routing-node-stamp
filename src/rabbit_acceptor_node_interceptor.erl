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
%% The Original Code is RabbitMQ Acceptor Node.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_acceptor_node_interceptor).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("rabbit_acceptor_node.hrl").

-import(rabbit_basic, [extract_headers/1, header/2, prepend_table_header/3]).

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1]).

-rabbit_boot_step({?MODULE,
                   [{description, "acceptor node interceptor"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"acceptor node interceptor">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"acceptor node interceptor">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

init(_Ch) ->
  undefined.

description() ->
  [{description,
    <<"Adds name of receiving node to messages as they enter RabbitMQ">>}].

intercept(#'basic.publish'{} = Method, Content, _IState) ->
  DecodedContent = rabbit_binary_parser:ensure_content_decoded(Content),
  Content2 = set_acceptor_node(DecodedContent),
  {Method, Content2};

intercept(Method, Content, _VHost) ->
  {Method, Content}.

applies_to() ->
  ['basic.publish'].

%%----------------------------------------------------------------------------

set_acceptor_node(#content{properties = #'P_basic'{headers = undefined}} = Content) ->
  set_acceptor_node(Content, node());

set_acceptor_node(#content{properties = #'P_basic'{headers = Headers}} = Content) ->
  case header(?ACCEPTOR_NODE_HEADER, Headers) of
    undefined -> set_acceptor_node(Content, node());
    _ -> Content  % Do not overwrite an existing acceptor node.
  end.

set_acceptor_node(#content{properties = #'P_basic'{headers = Headers} = Props} = Content, Node) ->
  NewHeaders =
  prepend_table_header(
    ?ACCEPTOR_NODE_HEADER,
    [{?ACCEPTOR_NODE_KEY, longstr, list_to_binary(atom_to_list(Node))}],
    Headers
   ),
  Content#content{
    properties = Props#'P_basic'{headers = NewHeaders},
    properties_bin = none
   }.
