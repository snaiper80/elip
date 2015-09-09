-module(cidr_map).
-export ([new_cidr_map/0,
          add_network/3,
          add_networks/3,
          find_network/2]).

-record(cidr_map, { root = nil }).
-opaque cidr_map() :: #cidr_map{}.
-export_type([cidr_map/0]).

%% defines
-define(IPV4_LENGTH,     32).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API Function Definitions
%% ===================================================================
-spec new_cidr_map() -> cidr_map().
new_cidr_map() ->
    #cidr_map {
        root = eltrie:new_trie()
    }.

-spec add_network(string(), string(), cidr_map()) -> cidr_map().
add_network(Name, CidrStr, #cidr_map { root = Root } = Map) ->
    [Ip, MaskStr] = string:tokens(CidrStr, "/"),

    {ok, Addr} = inet_parse:address(Ip),
    Mask       = list_to_integer(MaskStr),

    Map#cidr_map{root = eltrie:insert_trie(Name, << (ip_to_integer(Addr) bsr (?IPV4_LENGTH - Mask)):Mask >>, Mask, Root) }.

-spec add_networks(string(), [string(), ...], cidr_map()) -> cidr_map().
add_networks(Name, CidrStrs, #cidr_map { root = _ } = Map) when is_list(CidrStrs) ->
    lists:foldl(fun(Cidr, M) -> add_network(Name, Cidr, M) end, Map, CidrStrs).

-spec find_network(tuple(), cidr_map()) -> {found, string()} | not_found.
find_network({I0, I1, I2, I3} = Ip, #cidr_map { root = R }) when is_integer(I0), I0 >= 0, I0 < 256,
                                                                 is_integer(I1), I1 >= 0, I1 < 256,
                                                                 is_integer(I2), I2 >= 0, I2 < 256,
                                                                 is_integer(I3), I3 >= 0, I3 < 256 ->
    eltrie:find_trie(ip_to_binary(Ip), ?IPV4_LENGTH, R).

%% ===================================================================
%% Internal
%% ===================================================================
ip_to_integer({A, B, C, D}) ->
    << V:32 >> = << A:8, B:8, C:8, D:8 >>,
    V.

ip_to_binary({A, B, C, D}) ->
    << A:8, B:8, C:8, D:8 >>.

%% ===================================================================
%% UNIT TEST
%% ===================================================================
-ifdef(TEST).

find_network_test() ->
    T1  = new_cidr_map(),
    %T1 = add_network("0", "1.0.0.1/1", T),
    T2 = add_networks("A", ["1.1.1.0/24", "1.1.3.0/24"], T1),
    T3 = add_network("B", "2.2.0.0/16", T2),

    ?assertMatch({found, "A"},  find_network({1,   1, 1,  10},  T3)),
    ?assertMatch({found, "A"},  find_network({1,   1, 3,  100}, T3)),
    ?assertMatch({found, "B"},  find_network({2,   2, 11, 11},  T3)),
    ?assertMatch(not_found,     find_network({1,   1, 2,  10},  T3)),
    ?assertMatch(not_found,     find_network({1,   2, 3,  100}, T3)),
    ?assertMatch(not_found,     find_network({127, 0, 0,  1},   T3)).

-endif.
