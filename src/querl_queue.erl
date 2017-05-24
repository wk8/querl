-module(querl_queue).

-export([
         new/0,
         in/3,
         out/2,
         empty/1,
         remove/2,
         update/3,
         in_or_update/4,
         foldl/3,
         size/1,
         clone/1,
         to_list/1
        ]).

-record(?MODULE, {
    linked_list_root         :: _LinkedListRoot,
    linked_list_root_version :: non_neg_integer(),
    map = #{}                :: #{Key :: any() => {_LinkedListPointer, Payload :: any()}}
}).

-include("querl.hrl").

-export_type([queue/0, queue/2]).
-type queue(Key, Payload) :: #?MODULE{map :: #{Key => {_LinkedListPointer, Payload}}}.
-type queue() :: queue(any(), any()).

%% @doc Creates a new queue
new() ->
    {LinkedListRoot, LinkedListRootVersion} = querl_linked_list:new(),
    #?MODULE{linked_list_root = LinkedListRoot,
             linked_list_root_version = LinkedListRootVersion}.

%% @doc Inserts a new item at the end of the queue
%% Errors out if the `Key' is already present
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec in(#?MODULE{}, Key :: any(), Payload :: any()) -> {ok, #?MODULE{}} | already_present_error().
in(#?MODULE{linked_list_root = LinkedListRoot,
            linked_list_root_version = LinkedListRootVersion,
            map = Map} = Rec, Key, Payload) ->
    case maps:find(Key, Map) of
        {ok, _} ->
            {error, {already_present, Key}};
        error ->
            {LinkedListNode, NewLinkedListRootVersion} =
            querl_linked_list:append(LinkedListRoot, LinkedListRootVersion, Key),

            NewMap = maps:put(Key, {LinkedListNode, Payload}, Map),
            NewRec = Rec#?MODULE{linked_list_root_version = NewLinkedListRootVersion,
                                 map = NewMap},
            {ok, NewRec}
    end.

%% @doc Pops the first `RequestedSize' items in the queue (not an error to
%% request more than currently present in the queue)
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec out(#?MODULE{}, RequestedSize :: non_neg_integer())
-> {#?MODULE{}, ActualSize :: non_neg_integer(), [{Key :: any(), Payload :: any()}]}.
out(#?MODULE{linked_list_root = LinkedListRoot,
            linked_list_root_version = LinkedListRootVersion,
            map = Map} = Rec, RequestedSize) ->
    {ReversedKeys, ActualSize, NewLinkedListRootVersion} =
    querl_linked_list:reversed_out(LinkedListRoot, LinkedListRootVersion, RequestedSize),

    {NewMap, KeysWithPayloads} = lists:foldl(
        fun(Key, {CurrentMap, CurrentKeysWithPayloads}) ->
            {_LinkedListNode, Payload} = maps:get(Key, CurrentMap),
            {maps:remove(Key, CurrentMap),
             [{Key, Payload} | CurrentKeysWithPayloads]}
        end,
        {Map, []},
        ReversedKeys
    ),

    NewRec = Rec#?MODULE{linked_list_root_version = NewLinkedListRootVersion,
                         map = NewMap},
    {NewRec, ActualSize, KeysWithPayloads}.

%% @doc Empties the queue of all its current items, and returns an empty queue
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec empty(#?MODULE{})
-> {#?MODULE{}, Size :: non_neg_integer(), [{Key :: any(), Payload :: any()}]}.
empty(#?MODULE{linked_list_root = LinkedListRoot,
               linked_list_root_version = LinkedListRootVersion,
               map = Map} = Rec) ->
    {ReversedKeys, Size, NewLinkedListRootVersion} =
    querl_linked_list:reversed_empty(LinkedListRoot, LinkedListRootVersion),

    KeysWithPayloads = lists:foldl(
        fun(Key, CurrentKeysWithPayloads) ->
            {_LinkedListNode, Payload} = maps:get(Key, Map),
            [{Key, Payload} | CurrentKeysWithPayloads]
        end,
        [],
        ReversedKeys
    ),

    NewRec = Rec#?MODULE{linked_list_root_version = NewLinkedListRootVersion,
                         map = #{}},
    {NewRec, Size, KeysWithPayloads}.

%% @doc Removes the item key-ed with `Key', if present
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec remove(#?MODULE{}, Key :: any())
-> {ok, #?MODULE{}, Payload :: any()} | {error, not_found}.
remove(#?MODULE{linked_list_root = LinkedListRoot,
                linked_list_root_version = LinkedListRootVersion,
                map = Map} = Rec, Key) ->
    case maps:find(Key, Map) of
        {ok, {LinkedListNode, Payload}} ->
            NewLinkedListRootVersion =
            querl_linked_list:remove(LinkedListRoot, LinkedListRootVersion, LinkedListNode),

            NewRec = Rec#?MODULE{linked_list_root_version = NewLinkedListRootVersion,
                                 map = maps:remove(Key, Map)},
            {ok, NewRec, Payload};
        error ->
            {error, not_found}
    end.

%% @doc Updates an existing item in the queue (just the payload, not the
%% position)
%% This is not a destructive operation
-spec update(#?MODULE{}, Key :: any(), NewPayload | PayloadFun)
-> {ok, #?MODULE{}, OldPayload :: any()} | {error, not_found}
     when NewPayload :: any(),
          PayloadFun :: fun((OldPayload :: any()) -> NewPayload :: any()).
update(#?MODULE{map = Map} = Rec, Key, NewPayloadOrFun) ->
    case maps:find(Key, Map) of
        {ok, {LinkedListNode, OldPayload}} ->
            NewMapValue = {LinkedListNode, update_payload(OldPayload, NewPayloadOrFun)},
            NewMap = maps:put(Key, NewMapValue, Map),
            {ok, Rec#?MODULE{map = NewMap}, OldPayload};
        error ->
            {error, not_found}
    end.

%% @doc Inserts an existing item at the end of the queue, or `update's it if
%% it's already present
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec in_or_update(#?MODULE{}, Key :: any(), PayloadFun :: fun((OldPayload :: any()) -> NewPayload :: any()), InitialPayload :: any())
    -> {#?MODULE{}, OldPayload :: any() | not_present_before}.
in_or_update(Rec, Key, NewPayloadOrFun, InitialPayload) ->
    case in(Rec, Key, InitialPayload) of
        {ok, NewRec} ->
            {NewRec, not_present_before};
        {error, {already_present, Key}} ->
            {ok, NewRec, OldPayload} = update(Rec, Key, NewPayloadOrFun),
            {NewRec, OldPayload}
    end.

%% @doc Folds items in order, same idea as `lists:foldl/3'
%% This is not a destructive operation
-spec foldl(#?MODULE{}, Fun :: fun((Key :: any(), Payload :: any(), AccIn :: any()) -> AccOut :: any()), Acc0 :: any())
    -> AccOut :: any().
foldl(#?MODULE{linked_list_root = LinkedListRoot,
               linked_list_root_version = LinkedListRootVersion,
               map = Map}, Fun, Acc0) ->
    lists:foldl(
        fun(Key, Acc) ->
            {_LinkedListNode, Payload} = maps:get(Key, Map),
            Fun(Key, Payload, Acc)
        end,
        Acc0,
        querl_linked_list:to_list(LinkedListRoot, LinkedListRootVersion)).

%% @doc This is not a destructive operation
-spec size(#?MODULE{}) -> non_neg_integer().
size(#?MODULE{map = Map}) -> maps:size(Map).

%% @doc Might seem slightly counter-intuitive to Erlang programmers, but
%% operations on queues actually change the underlying object without cloning
%% it...
%% This operation is costly: it basically rebuilds the whole thing from
%% scratch; shouldn't be used in production code
-spec clone(#?MODULE{}) -> #?MODULE{}.
clone(#?MODULE{linked_list_root = LinkedListRoot,
               linked_list_root_version = LinkedListRootVersion,
               map = Map}) ->
    lists:foldl(
        fun(Key, CurrentClone) ->
            {_LinkedListNode, Payload} = maps:get(Key, Map),
            {ok, NewClone} = in(CurrentClone, Key, Payload),
            NewClone
        end,
        new(),
        querl_linked_list:to_list(LinkedListRoot, LinkedListRootVersion)
    ).

%% @doc Returns a list of the contents of the list, in order
%% This is not a destructive operation
-spec to_list(#?MODULE{}) -> [{Key :: any(), Payload :: any()}].
to_list(#?MODULE{linked_list_root = LinkedListRoot,
                 linked_list_root_version = LinkedListRootVersion,
                 map = Map}) ->
    lists:foldl(
        fun(Key, CurrentList) ->
            {_LinkedListNode, Payload} = maps:get(Key, Map),
            [{Key, Payload} | CurrentList]
        end,
        [],
        querl_linked_list:to_reversed_list(LinkedListRoot, LinkedListRootVersion)
    ).

%%% Private helpers

%% @private
update_payload(OldPayload, PayloadFun) when erlang:is_function(PayloadFun) ->
    PayloadFun(OldPayload);
update_payload(_OldPayload, NewPayload) ->
    NewPayload.
