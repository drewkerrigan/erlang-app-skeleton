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

-module(sampler_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-include("sampler.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
    SAMPLER_SERVER = {sampler_server,
          {sampler_server, start_link, [[]]},
          permanent, 5000, worker, [sampler_server]},
    WEB = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [web_config()]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [SAMPLER_SERVER, WEB],

    {ok, { {one_for_one, 10, 10}, Processes} }.

%%%===================================================================
%%% Private
%%%===================================================================

web_config() ->
    {Ip, Port} = sampler:web_host_port(),
    [
        {ip, Ip},
        {port, Port},
        {nodelay, true},
        {log_dir, "log"},
        {dispatch, dispatch()}
    ].

dispatch() ->
    Resources = [
        sampler_wm_resource:dispatch()
    ],
    lists:flatten(Resources).
