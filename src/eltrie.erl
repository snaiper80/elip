%% Ref Data Structure Description
%%      http://opendatastructures.org/ods-java/13_1_BinaryTrie_digital_sea.html
%% Original Source URL :
%%      https://github.com/GaoYusong/benchmark_ipfilter/blob/master/main.erl
-module(eltrie).

-export ([new_trie/0,
          insert_trie/4,
          find_trie/3]).

new_trie() ->
    nil.

insert_trie(Name, << >>, 0, {non_leaf, LT, RT}) ->
    {leaf, Name, LT, RT};
insert_trie(Name, Val, Len, nil) ->
    insert_trie(Name, Val, Len, {non_leaf, nil, nil});
insert_trie(Name, Val, Len, {non_leaf, LT, RT}) ->
    RestLen = Len - 1,
    case Val of
        << 0:1, R:RestLen >> ->
            {non_leaf, insert_trie(Name, << R:RestLen >>, RestLen, LT), RT};
        << 1:1, R:RestLen >> ->
            {non_leaf, LT, insert_trie(Name, << R:RestLen >>, RestLen, RT)}
    end.

find_trie(_, _, nil) ->
    not_found;
find_trie(_, _, {leaf, Name, _, _}) ->
    {found, Name};
find_trie(<< >>, 0, {non_leaf, _, _}) ->
    not_found;
find_trie(Val, Len, {non_leaf, LT, RT}) ->
    RestLen = Len - 1,
    case Val of
        << 0:1, R:RestLen >> ->
            find_trie(<< R:RestLen >>, RestLen, LT);
        << 1:1, R:RestLen >> ->
            find_trie(<< R:RestLen >>, RestLen, RT)
    end.