%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Drew Kerrigan All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(sampler_wm_resource).
-export([routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         provide_content/2,
         accept_content/2]).

-record(ctx, {response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("sampler.hrl").

%%%===================================================================
%%% API
%%%===================================================================

routes() ->
     [[?SAMPLER_BASE_ROUTE] ++ ["status"]].

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx) ->
    {true, RD, Ctx}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET','PUT'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content}],
    {Types, RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    Types = [{"application/json", accept_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx) ->
    case sampler_server:get_status() of
        {error, _Reason} ->
            {false, RD, Ctx};
        Response ->
            {true, RD, Ctx#ctx{response=Response}}
    end.

provide_content(RD, Ctx=#ctx{response=Response}) ->
    render_json(Response, RD, Ctx).

accept_content(RD, Ctx) ->
    RawValue = wrq:req_body(RD),
    case mochijson2:decode(RawValue) of
        {struct, [{<<"status">>, Status}]} ->
            sampler_server:set_status([{status, Status}]),
            {true, RD, Ctx#ctx{response=undefined}};
        _ ->
            {false, RD, Ctx#ctx{response=undefined}}
    end.

%% ====================================================================
%% Private
%% ====================================================================

render_json(Data, RD, Ctx) ->
    Body = mochijson2:encode(Data),
    {Body, RD, Ctx}.
