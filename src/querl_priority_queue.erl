-module(querl_priority_queue).

-export([
         new/1,
         in/4,
         out/2,
         out/4,
         empty/1,
         empty/3,
         remove/2,
         remove/3,
         update_priority/3,
         update_priority/4,
         size/1,
         size/2,
         lowest_priority/1,
         clone/1
        ]).

-type priority() :: non_neg_integer().

-record(?MODULE, {
           queues = #{}     :: #{priority() => querl_queue:queue(Key :: any(), Payload :: any())},
           priorities = #{} :: #{Key :: any() => priority()},
           size = 0         :: non_neg_integer()
          }).

-export_type([queue/0, queue/2]).
-type queue(Key, Payload) :: #?MODULE{queues     :: #{priority() => querl_queue:queue(Key, Payload)},
                                      priorities :: #{Key => priority()}}.
-type queue() :: queue(any(), any()).

%% @doc Creates a new queue with lowest priority `LowestPriority'
-spec new(LowestPriority :: priority()) -> #?MODULE{}.
new(LowestPriority) ->
    Queues = [{Priority, querl_queue:new()} || Priority <- lists:seq(1, LowestPriority)],
    #?MODULE{queues = maps:from_list(Queues)}.

%% @doc Adds a new item in the queue
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec in(#?MODULE{}, Key :: any(), Priority :: priority(), Payload :: any())
-> {ok, #?MODULE{}} | {error, already_present}.
in(#?MODULE{queues = Queues, priorities = Priorities, size = Size}, Key, Priority, Payload) ->
    case maps:is_key(Key, Priorities) of
        false ->
            NewQueues = maps:update_with(
                Priority,
                fun(Queue) ->
                    {ok, NewQueue} = querl_queue:in(Queue, Key, Payload),
                    NewQueue
                end,
                Queues),
            NewPriorities = maps:put(Key, Priority, Priorities),
            {ok, #?MODULE{queues     = NewQueues,
                          priorities = NewPriorities,
                          size       = Size + 1}};
        true ->
            {error, already_present}
    end.

%% @doc Pops up to `Size' items out of the queue (not an error to request more
%% than the actual size of the queue)
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec out(#?MODULE{}, RequestedSize :: non_neg_integer())
-> {#?MODULE{}, ActualSize :: non_neg_integer(), [{Key :: any(), Priority :: priority(), Payload :: any()}]}.
out(Rec, RequestedSize) ->
    out(Rec, RequestedSize, 1, lowest_priority(Rec) + 1).

%% @doc Same as `out/2', except will only pop items with priorities gte
%% `FromPriority' and lt `ToPriority'
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec out(#?MODULE{}, RequestedSize :: non_neg_integer(), FromPriority :: priority(), ToPriority :: priority())
-> {#?MODULE{}, ActualSize :: non_neg_integer(), [{Key :: any(), Priority :: priority(), Payload :: any()}]}.
out(#?MODULE{queues = Queues, priorities = Priorities, size = Size},
        RequestedSize, FromPriority, ToPriority) ->
    out(Queues, Priorities, Size, RequestedSize, 0, [], FromPriority, ToPriority).

%% @private
out(Queues, Priorities, Size, RequestedSize, ActualSize, ReversedList, CurrentPriority, ToPriority)
  when RequestedSize =< 0 orelse CurrentPriority >= ToPriority ->
    NewRec = #?MODULE{queues = Queues, priorities = Priorities, size = Size},

    {NewRec, ActualSize, lists:reverse(ReversedList)};
out(Queues, Priorities, Size, RequestedSize, ActualSize, ReversedList, CurrentPriority, ToPriority) ->
    Queue = maps:get(CurrentPriority, Queues),
    {NewQueue, SizeOut, KeysAndPayloads} = querl_queue:out(Queue, RequestedSize),
    NewQueues = maps:put(CurrentPriority, NewQueue, Queues),

    {NewPriorities, NewReversedList} =
    process_keys_and_payloads(KeysAndPayloads, Priorities, CurrentPriority, ReversedList),

    out(NewQueues, NewPriorities, Size - SizeOut, RequestedSize - SizeOut,
        ActualSize + SizeOut, NewReversedList, CurrentPriority + 1, ToPriority).

%% @doc Empties the queue of all its current items, and returns an empty queue
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec empty(#?MODULE{})
-> {#?MODULE{}, ActualSize :: non_neg_integer(), [{Key :: any(), Priority :: priority(), Payload :: any()}]}.
empty(Rec) -> empty(Rec, 1, lowest_priority(Rec) + 1).

%% @doc Same as `empty/1', except only empties priorities gte `FromPriority' and
%% lt `ToPriority'
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec empty(#?MODULE{}, FromPriority :: priority(), ToPriority :: priority())
-> {#?MODULE{}, ActualSize :: non_neg_integer(), [{Key :: any(), Priority :: priority(), Payload :: any()}]}.
empty(#?MODULE{queues = Queues, priorities = Priorities, size = Size}, FromPriority, ToPriority) ->
    empty(Queues, Priorities, Size, 0, [], FromPriority, ToPriority).

%% @private
empty(Queues, Priorities, Size, ActualSize, ReversedList, CurrentPriority, ToPriority)
  when CurrentPriority >= ToPriority ->
    NewRec = #?MODULE{queues = Queues, priorities = Priorities, size = Size},

    {NewRec, ActualSize, lists:reverse(ReversedList)};
empty(Queues, Priorities, Size, ActualSize, ReversedList, CurrentPriority, ToPriority) ->
    Queue = maps:get(CurrentPriority, Queues),
    {NewQueue, SizeOut, KeysAndPayloads} = querl_queue:empty(Queue),
    NewQueues = maps:put(CurrentPriority, NewQueue, Queues),

    {NewPriorities, NewReversedList} =
    process_keys_and_payloads(KeysAndPayloads, Priorities, CurrentPriority, ReversedList),

    empty(NewQueues, NewPriorities, Size - SizeOut, ActualSize + SizeOut,
          NewReversedList, CurrentPriority + 1, ToPriority).

%% @private
-spec process_keys_and_payloads([{Key, Payload}], #{Key => priority()}, priority(), [{Key, priority(), Payload}])
-> {#{Key => priority()}, [{Key, priority(), Payload}]}
     when Key :: any(), Payload :: any().
process_keys_and_payloads(KeysAndPayloads, Priorities, CurrentPriority, ReversedList) ->
    lists:foldl(
        fun({Key, Payload}, {CurrentPriorities, CurrentReversedList}) ->
            {maps:remove(Key, CurrentPriorities),
             [{Key, CurrentPriority, Payload} | CurrentReversedList]}
        end,
        {Priorities, ReversedList},
        KeysAndPayloads).

%% @doc Removes an item from the queue, and returns its previous priority and
%% payload
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec remove(#?MODULE{}, Key :: any()) -> {ok, #?MODULE{}, Priority :: priority(), Payload :: any()} | {error, not_found}.
remove(#?MODULE{queues = Queues, priorities = Priorities, size = Size}, Key) ->
    case maps:find(Key, Priorities) of
        {ok, Priority} ->
            Queue = maps:get(Priority, Queues),
            {ok, NewQueue, Payload} = querl_queue:remove(Queue, Key),

            {ok, #?MODULE{queues     = maps:put(Priority, NewQueue, Queues),
                          priorities = maps:remove(Key, Priorities),
                          size       = Size - 1},
                 Priority, Payload};
        error ->
            {error, not_found}
    end.

%% @doc Same as `remove/2', providing the `Priority' too (the item won't
%% be found if it's present in the queue with a different priority)
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec remove(#?MODULE{}, Key :: any(), Priority :: priority())
-> {ok, #?MODULE{}, Payload :: any()} | {error, not_found}.
remove(#?MODULE{queues = Queues, priorities = Priorities, size = Size}, Key, Priority) ->
    Queue = maps:get(Priority, Queues),
    case querl_queue:remove(Queue, Key) of
        {ok, NewQueue, Payload} ->
            {ok, #?MODULE{queues     = maps:put(Priority, NewQueue, Queues),
                          priorities = maps:remove(Key, Priorities),
                          size       = Size - 1},
                 Payload};
        {error, not_found} = NotFound ->
            NotFound
    end.

%% @doc Changes the priority of an item already present in the queue (doesn't
%% touch the payload, nor does it change the affect the item's position in the
%% list if it already has the same priority - if the priority is changed, the
%% item is moved at the end of its new priority)
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec update_priority(#?MODULE{}, Key :: any(), NewPriority :: priority())
-> {ok, #?MODULE{}, OldPriority :: priority()} | {error, not_found}.
update_priority(#?MODULE{queues = Queues, priorities = Priorities} = Rec, Key, NewPriority) ->
    case maps:find(Key, Priorities) of
        {ok, NewPriority} ->
            %% nothing to do, already in the right place!
            {ok, Rec, NewPriority};
        {ok, OldPriority} ->
            FromQueue = maps:get(OldPriority, Queues),
            {ok, NewFromQueue, Payload} = querl_queue:remove(FromQueue, Key),

            ToQueue = maps:get(NewPriority, Queues),
            {ok, NewToQueue} = querl_queue:in(ToQueue, Key, Payload),

            NewQueues1 = maps:put(OldPriority, NewFromQueue, Queues),
            NewQueues2 = maps:put(NewPriority, NewToQueue, NewQueues1),

            NewPriorities = maps:put(Key, NewPriority, Priorities),

            NewRec = Rec#?MODULE{queues = NewQueues2, priorities = NewPriorities},
            {ok, NewRec, OldPriority};
        error ->
            {error, not_found}
    end.

%% @doc Same as `update_priority/3', providing the `CurrentPriority' too (the
%% item won't be found if it's present in the queue with a different priority)
%% The behaviour is the same
%% WARNING: this is a destructive operation! see `clone/1' below for more
%% details
-spec update_priority(#?MODULE{}, Key :: any(), CurrentPriority :: priority(), NewPriority :: priority())
-> {ok, #?MODULE{}} | {error, not_found}.
update_priority(#?MODULE{queues = Queues, priorities = Priorities} = Rec, Key, CurrentPriority, NewPriority) ->
    FromQueue = maps:get(CurrentPriority, Queues),
    case querl_queue:remove(FromQueue, Key) of
        {ok, NewFromQueue, Payload} ->
            case CurrentPriority =:= NewPriority of
                true ->
                    {ok, Rec};
                false ->
                    ToQueue = maps:get(NewPriority, Queues),
                    {ok, NewToQueue} = querl_queue:in(ToQueue, Key, Payload),

                    NewQueues1 = maps:put(CurrentPriority, NewFromQueue, Queues),
                    NewQueues2 = maps:put(NewPriority, NewToQueue, NewQueues1),

                    NewPriorities = maps:put(Key, NewPriority, Priorities),

                    NewRec = Rec#?MODULE{queues = NewQueues2, priorities = NewPriorities},
                    {ok, NewRec}
            end;
        {error, not_found} = NotFound ->
            NotFound
    end.

%% @doc This is not a destructive operation
-spec size(#?MODULE{}) -> non_neg_integer().
size(#?MODULE{size = Size}) -> Size.

%% @doc The size for a given priority; this is not a destructive operation
-spec size(#?MODULE{}, Priority :: priority()) -> non_neg_integer().
size(#?MODULE{queues = Queues}, Priority) ->
    querl_queue:size(maps:get(Priority, Queues)).

%% @doc Returns the queue's lowest priority (that was passed at initialization
%% time)
%% This is not a destructive operation
-spec lowest_priority(#?MODULE{}) -> priority().
lowest_priority(#?MODULE{queues = Queues}) -> maps:size(Queues).

%% @doc Same idea as for `querl_queue:clone/1'; shouldn't be used in production
%% code!
-spec clone(#?MODULE{}) -> #?MODULE{}.
clone(#?MODULE{queues = Queues} = Rec) ->
    ClonedQueues = maps:map(
        fun(_Priority, Queue) -> querl_queue:clone(Queue) end,
        Queues),
    Rec#?MODULE{queues = ClonedQueues}.
