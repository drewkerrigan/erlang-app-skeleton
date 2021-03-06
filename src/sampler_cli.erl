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

-module(sampler_cli).

-behaviour(clique_handler).

-export([command/1]).

-export([
    register/0,
    load_schema/0]).

-export([
    status/3
]).

-export([register_cli/0]).

%%%===================================================================
%%% API
%%%===================================================================

command(Cmd) ->
    clique:run(Cmd).

register() ->
    clique:register([?MODULE]).

load_schema() ->
    case application:get_env(sampler, schema_dirs) of
        {ok, Directories} ->
            ok = clique_config:load_schema(Directories);
        _ ->
            ok = clique_config:load_schema([code:lib_dir()])
    end.

status(_Command, [], []) ->
    JSON = sampler_server:get_status(),
    [clique_status:text(mochijson2:encode(JSON))];
status(_Command, [], [{status, Status}]) ->
    sampler_server:set_status([{status, list_to_binary(Status)}]),
    JSON = sampler_server:get_status(),
    [clique_status:text(mochijson2:encode(JSON))].


%%%===================================================================
%%% Callbacks
%%%===================================================================

register_cli() ->
    clique:register_usage(["sampler-admin"], usage()),
    clique:register_command(["sampler-admin", "status"],[],
        [{status, [{shortname, "s"}, {longname, "status"}]}],fun status/3).

%%%===================================================================
%%% Private
%%%===================================================================

usage() ->
    [
     "sampler-admin <sub-command>\n\n",
     "  Interact with a running sampler app.\n\n",
     "  Sub-commands:\n",
     "    status [-s NewStatus]         Get the current status. Optionally set a new status with the -s flag.\n",
     "  Use --help after a sub-command for more details.\n"
    ].
