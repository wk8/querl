-module(querl_queue_tests).

-include_lib("eunit/include/eunit.hrl").

%% all the basic funs are pretty inter-dependent, to the point that it hardly
%% makes sense to test each individually
basic_usage_test() ->
    Queue1 = querl_queue:new(),

    %% let's insert a bunch of things in the queue
    KeysAndPayloads1 = [{I, I * 2} || I <- lists:seq(10, 1, -1)],

    Queue2 = lists:foldl(
               fun({Key, Payload}, CurrentQueue) ->
                       {ok, NewQueue} = querl_queue:in(CurrentQueue, Key, Payload),
                       NewQueue
               end,
               Queue1,
               KeysAndPayloads1
              ),

    assert_queue_equals(KeysAndPayloads1, Queue2),

    %% now trying to re-add an existing key should error out
    ?assertEqual({error, already_present}, querl_queue:in(Queue2, 6, blah)),

    %% now let's remove all even keys
    {EvenKeysAndPayloads, OddKeysAndPayloads} =
    lists:partition(fun({Key, _Payload}) -> Key rem 2 =:= 0 end, KeysAndPayloads1),

    Queue3 = lists:foldl(
               fun({EvenKey, Payload}, CurrentQueue) ->
                       {ok, NewQueue, Payload} = querl_queue:remove(CurrentQueue, EvenKey),
                       NewQueue
               end,
               Queue2,
               EvenKeysAndPayloads
              ),

    assert_queue_equals(OddKeysAndPayloads, Queue3),

    %% at this point, trying to remove an even key should yield an error
    ?assertEqual({error, not_found}, querl_queue:remove(Queue3, 10)),

    %% let's re-add 10 and 2 at the end of the queue
    KeysAndPayloads2 = OddKeysAndPayloads ++ [{2, 12}, {10, 28}],
    {ok, Queue4} = querl_queue:in(Queue3, 2, 12),
    {ok, Queue5} = querl_queue:in(Queue4, 10, 28),
    assert_queue_equals(KeysAndPayloads2, Queue5),

    %% let's update a few items ({Key, ExpectedOldPayload, NewPayload})
    Updates1 = [{7, 14, 82}, {2, 12, 96}, {1, 2, 100}],
    Queue6 = lists:foldl(
               fun({Key, ExpectedOldPayload, NewPayload}, CurrentQueue) ->
                       {ok, NewQueue, ActualOldPayload} =
                       querl_queue:update(CurrentQueue, Key, NewPayload),

                       ?assertEqual(ExpectedOldPayload, ActualOldPayload),

                       NewQueue
               end,
               Queue5,
               Updates1
              ),
    KeysAndPayloads3 = lists:map(
                         fun({Key, Payload} = KeyAndPayload) ->
                                 case lists:keyfind(Key, 1, Updates1) of
                                     {Key, Payload, NewPayload} -> {Key, NewPayload};
                                     false -> KeyAndPayload
                                 end
                         end,
                         KeysAndPayloads2
                        ),
    assert_queue_equals(KeysAndPayloads3, Queue6),

    %% trying to update a deleted item, or an item that was never there, should
    %% yield errors
    ?assertEqual({error, not_found}, querl_queue:update(Queue6, 8, whatever)),
    ?assertEqual({error, not_found}, querl_queue:update(Queue6, -1, whatever)),

    %% we can also update using a fun
    UpdateFun = fun(Payload) -> -Payload * 3 end,
    Queue7 = lists:foldl(
               fun({Key, ExpectedOldPayload}, CurrentQueue) ->
                       {ok, NewQueue, ActualOldPayload} =
                       querl_queue:update(CurrentQueue, Key, UpdateFun),

                       ?assertEqual(ExpectedOldPayload, ActualOldPayload),

                       NewQueue
               end,
               Queue6,
               KeysAndPayloads3
              ),
    KeysAndPayloads4 = lists:map(
                         fun({Key, Payload}) -> {Key, UpdateFun(Payload)} end,
                         KeysAndPayloads3
                        ),
    assert_queue_equals(KeysAndPayloads4, Queue7),

    %% at this point, the queue contains
    %% [{9,-54},{7,-246},{5,-30},{3,-18},{1,-300},{2,-288},{10,-84}]
    %% (trust me)

    %% let's test `in_or_update/4' in the 3 possible different configurations:
    %% first, with a totally new item
    {Queue8, not_present_before} =
    querl_queue:in_or_update(Queue7, -28, UpdateFun, 42),
    %% then with an item which had been previously deleted
    {Queue9, not_present_before} =
    querl_queue:in_or_update(Queue8, 4, UpdateFun, 67),
    %% and lastly, with an already present item
    {Queue10, -54} = querl_queue:in_or_update(Queue9, 9, UpdateFun, whatever),

    KeysAndPayloads5 = lists:map(
                         fun({9, Payload}) -> {9, UpdateFun(Payload)};
                            (Else) -> Else
                         end,
                         KeysAndPayloads4
                        ) ++ [{-28, 42}, {4, 67}],
    assert_queue_equals(KeysAndPayloads5, Queue10),

    %% so now the queue contains:
    %% [{9,162},{7,-246},{5,-30},{3,-18},{1,-300},{2,-288},
    %%  {10,-84},{-28,42},{4,67}]

    %% then let's remove the first three elements
    {Queue11, 3, _} = querl_queue:out(Queue10, 3),
    {_, KeysAndPayloads6} = lists:split(3, KeysAndPayloads5),

    assert_queue_equals(KeysAndPayloads6, Queue11),

    %% and let's fold on what's left, i.e.
    ExpectedSum = -581,
    ActualSum = querl_queue:foldl(
      Queue11,
      fun(_Key, Payload, CurrentSum) -> CurrentSum + Payload end,
      0
     ),
    ?assertEqual(ExpectedSum, ActualSum).

parallel_usage_test_() ->
    ProcessesCount = 20,
    Self = erlang:self(),

    ExpectedSortedMessages = [{ok, basic_usage_test, I}
                              || I <- lists:seq(1, ProcessesCount)],
    [erlang:spawn(fun() ->
        basic_usage_test(),

        Self ! Message
    end) || Message <- ExpectedSortedMessages],

    ActualMessages = receive_basic_usage_test_messages(ProcessesCount),

    ?_assertEqual(ExpectedSortedMessages, lists:sort(ActualMessages)).

destructive_operations_test_() ->
    EmptyQueue = querl_queue:new(),
    {ok, BaseQueue} = querl_queue:in(EmptyQueue, 42, payload),
    {ok, _Queue} = querl_queue:in(BaseQueue, 2, another_payload),

    %% all of these should fail because we're trying to re-use the `BaseQueue'
    %% after performing a destructive operation on it
    [?_assertError(badarg, querl_queue:in(BaseQueue, 43, other_payload)),
     ?_assertError(badarg, querl_queue:out(BaseQueue, 1)),
     ?_assertError(badarg, querl_queue:empty(BaseQueue)),
     ?_assertError(badarg, querl_queue:remove(BaseQueue, 42)),
     ?_assertError(badarg, querl_queue:in_or_update(BaseQueue, 43, other_payload, initial_payload)),
     ?_assertError(badarg, querl_queue:foldl(BaseQueue, fun() -> whatever end, acc0)),
     ?_assertError(badarg, querl_queue:clone(BaseQueue)),
     ?_assertError(badarg, querl_queue:to_list(BaseQueue))].

%%% Private helpers

%% @private
assert_queue_equals(ExpectedKeysAndPayloads, Queue) ->
    assert_queue_equals_without_sublists(ExpectedKeysAndPayloads, Queue),

    ExpectedSize = erlang:length(ExpectedKeysAndPayloads),

    %% now let's try `out'-ing all the possible sublengths
    lists:foreach(
      fun(RequestedSize) ->
              {ExpectedKeysAndPayloadsFromOut, ExpectedRemainingKeysAndPayloads} =
              lists:split(RequestedSize, ExpectedKeysAndPayloads),

              Clone = querl_queue:clone(Queue),
              {QueueOut, ActualSize, ActualKeysAndPayloadsFromOut} =
              querl_queue:out(Clone, RequestedSize),

              ?assertEqual(RequestedSize, ActualSize),
              ?assertEqual(ExpectedKeysAndPayloadsFromOut, ActualKeysAndPayloadsFromOut),

              assert_queue_equals_without_sublists(ExpectedRemainingKeysAndPayloads, QueueOut)
      end,
      lists:seq(0, ExpectedSize)
     ),

    %% and finally let's try `out'-ing more than the size
    Clone = querl_queue:clone(Queue),
    {EmptyQueue, ActualTotalSize, ActualKeysAndPayloads} =
    querl_queue:out(Clone, ExpectedSize + 1),

    ?assertEqual(ExpectedSize, ActualTotalSize),
    ?assertEqual(ExpectedKeysAndPayloads, ActualKeysAndPayloads),
    assert_queue_empty(EmptyQueue).

%% @private
assert_queue_equals_without_sublists(ExpectedKeysAndPayloads, Queue) ->
    ExpectedSize = erlang:length(ExpectedKeysAndPayloads),

    Clone = querl_queue:clone(Queue),
    {EmptyQueue, ActualSize1, ActualKeysAndPayloads1} = querl_queue:empty(Clone),
    ActualSize2 = querl_queue:size(Queue),
    ActualKeysAndPayloads2 = querl_queue:to_list(Queue),

    ?assertEqual(ExpectedSize, ActualSize1),
    ?assertEqual(ExpectedSize, ActualSize2),
    ?assertEqual(ExpectedKeysAndPayloads, ActualKeysAndPayloads1),
    ?assertEqual(ExpectedKeysAndPayloads, ActualKeysAndPayloads2),

    assert_queue_empty(EmptyQueue).

%% @private
assert_queue_empty(Queue) ->
    Clone = querl_queue:clone(Queue),
    ?assertMatch({_, 0, []}, querl_queue:empty(Clone)),
    ?assertEqual(0, querl_queue:size(Queue)),
    ?assertEqual([], querl_queue:to_list(Queue)).

%% @private
receive_basic_usage_test_messages(ExpectedCount) ->
    receive_basic_usage_test_messages(ExpectedCount, 0, []).

%% @private
receive_basic_usage_test_messages(ExpectedCount, ExpectedCount, Messages) ->
    Messages;
receive_basic_usage_test_messages(ExpectedCount, CurrentCount, Messages) ->
    receive {ok, basic_usage_test, _} = Message ->
        receive_basic_usage_test_messages(ExpectedCount, CurrentCount + 1, [Message | Messages])
    after 500 -> Messages end.
