-module(elip).
-behaviour(gen_server).

-export([start/0,
         start_link/1]).

-export([lookup/1]).

%% Defines
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

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [], []).

-spec lookup(string() | {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()})
        -> {ok, {found, string()} | not_found}.
lookup(IpStr) when is_list(IpStr) ->
    {ok, Addr} = inet_parse:address(IpStr),
    gen_server:call(get_worker(Addr), {lookup, Addr});
lookup(Ip) when is_tuple(Ip), size(Ip) =:= 4 ->
    gen_server:call(get_worker(Ip), {lookup, Ip}).

%% ===================================================================
%% gen_server Function Definitions
%% ===================================================================
init(_Args) ->
    process_flag(trap_exit, true),

    {ok, #state {
        cidr_map = load_cidr_map(default_config_path())
    }}.

handle_call({lookup, {I0, I1, I2, I3} = Ip}, _, State) when is_integer(I0), I0 >= 0, I0 < 256,
                                                            is_integer(I1), I1 >= 0, I1 < 256,
                                                            is_integer(I2), I2 >= 0, I2 < 256,
                                                            is_integer(I3), I3 >= 0, I3 < 256 ->
    {reply, {ok, cidr_map:find_network(Ip, State#state.cidr_map)}, State};
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
get_worker(Address) ->
    W = elip_sup:workers(),
    element(1 + erlang:phash2(Address, tuple_size(W)), W).

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


