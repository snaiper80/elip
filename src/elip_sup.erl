-module(elip_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([workers/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, workers_spec()} }.

workers() ->
    {elip_1, elip_2, elip_3, elip_4, elip_5}.

workers_spec() ->
    [{Id,
        {elip, start_link, [Id]}, permanent, 5000, worker, [elip]
     } || Id <- tuple_to_list(workers())].