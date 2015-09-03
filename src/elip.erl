-module(elip).
-behaviour(gen_server).

-export([start/0,
         start_link/0]).

-export([lookup/1]).

%% Defines
-define(SERVER,                    ?MODULE).
-define(DEFAULT_CONFIG_FILENAME,   "networks.config").

%% Records
-record (state, {
        cidr_map = null
    }).

%% ===================================================================
%% gen_server Function Exports
%% ===================================================================
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ===================================================================
%% API Function Definitions
%% ===================================================================
start() ->
    application:start(elip).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec lookup(string()) -> {ok, {found, string()} | not_found}.
lookup(IpStr) ->
    gen_server:call(?MODULE, {lookup, IpStr}).

%% ===================================================================
%% gen_server Function Definitions
%% ===================================================================
init(_Args) ->
    process_flag(trap_exit, true),

    {ok, #state {
        cidr_map = load_cidr_map(default_config_path())
    }}.

handle_call({lookup, IpStr}, _From, #state { cidr_map = CidrMap } = State) ->
    {ok, Addr} = inet_parse:address(IpStr),
    {reply,
        {ok, cidr_map:find_network(Addr, CidrMap)},
     State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
default_config_path() ->
  case code:priv_dir(?MODULE) of
      {error, bad_name} ->
          filename:join([
              filename:dirname(code:which(?MODULE)),
              "..",
              "priv",
              ?DEFAULT_CONFIG_FILENAME
          ]);
      Dir ->
          filename:join([Dir, ?DEFAULT_CONFIG_FILENAME])
  end.

load_cidr_map(File) ->
    {ok, Contents} = file:consult(File),
    error_logger:info_msg("Loading network config from ~p~n", [File]),
    lists:foldl(fun load_network/2, cidr_map:new_cidr_map(), Contents).

load_network({ {name, Name}, {description, Desc}, {network, SubnetIps} }, CidrMap) ->
    error_logger:info_msg("~s(~s) is loading with ~p.~n", [Name, Desc, SubnetIps]),
    cidr_map:add_networks(Name, SubnetIps, CidrMap).


