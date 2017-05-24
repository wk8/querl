-module(querl_priority_queue_tests).

-include_lib("eunit/include/eunit.hrl").

basic_usage_test() ->
    Queue1 = querl_priority_queue:new(3),

    %% we add 10-19 to priority 1, 20-29 to 2, and 30-39 to 3
    %% all with values being twice the key
    KeysAndPrioritiesAndPayloads1 =
    lists:flatten(lists:map(
                    fun(CurrentPriority) ->
                            lists:map(
                              fun(CurrentKey) ->
                                      {CurrentKey, CurrentPriority, CurrentKey * 2}
                              end,
                              lists:seq(CurrentPriority * 10, CurrentPriority * 10 + 9)
                             )
                    end,
                    lists:seq(1, 3)
                   )),
    Queue2 = lists:foldl(
               fun({Key, Priority, Payload}, CurrentQueue) ->
                       {ok, NewQueue} = querl_priority_queue:in(CurrentQueue, Key, Priority, Payload),
                       NewQueue
               end,
               Queue1,
               KeysAndPrioritiesAndPayloads1
              ),
    assert_queue_equals(KeysAndPrioritiesAndPayloads1, Queue2),

    %% now trying to re-add an existing key should error out
    ?assertEqual({error, {already_present, 16}},
                 querl_priority_queue:in(Queue2, 16, 3, blah)),

    %% now let's remove all even keys
    {EvenKeysAndPrioritiesAndPayloads, OddKeysAndPrioritiesAndPayloads} =
    lists:partition(fun({Key, _Priority, _Payload}) -> Key rem 2 =:= 0 end, KeysAndPrioritiesAndPayloads1),

    Queue3 = lists:foldl(
               fun({EvenKey, Priority, Payload}, CurrentQueue) ->
                       {ok, NewQueue, Priority, Payload} =
                       querl_priority_queue:remove(CurrentQueue, EvenKey),

                       NewQueue
               end,
               querl_priority_queue:clone(Queue2),
               EvenKeysAndPrioritiesAndPayloads
              ),
    assert_queue_equals(OddKeysAndPrioritiesAndPayloads, Queue3),

    %% trying to remove a key that never existed or else just got removed should
    %% result in explicit errors
    ?assertEqual({error, not_found},
                 querl_priority_queue:remove(Queue3, -1)),
    ?assertEqual({error, not_found},
                 querl_priority_queue:remove(Queue3, 16)),

    %% mind you, should be possible to do the exact same thing with `remove/3'
    Queue3Bis = lists:foldl(
                  fun({EvenKey, Priority, Payload}, CurrentQueue) ->
                          {ok, NewQueue, Payload} =
                          querl_priority_queue:remove(CurrentQueue, EvenKey, Priority),

                          NewQueue
                  end,
                  querl_priority_queue:clone(Queue2),
                  EvenKeysAndPrioritiesAndPayloads
                 ),
    assert_queue_equals(OddKeysAndPrioritiesAndPayloads, Queue3Bis),

    ?assertEqual({error, not_found},
                 querl_priority_queue:remove(Queue3Bis, -1, 1)),
    ?assertEqual({error, not_found},
                 querl_priority_queue:remove(Queue3Bis, 16, 1)),

    %% additionally, trying to remove with `remove/3' with the wrong priority
    %% should also yield an error
    ?assertEqual({error, not_found},
                 querl_priority_queue:remove(Queue3, 25, 1)),

    %% now let's re-add the even keys for priority 2; they will go to the end of
    %% the priority-2 queue though
    ReAddedKeysAndPrioritiesAndPayloads = lists:map(
                                            fun(Key) -> {Key, 2, Key * 2} end,
                                            lists:seq(20, 28, 2)
                                           ),
    {OddKeysAndPrioritiesAndPayloadsPriorities1And2, OddKeysAndPrioritiesAndPayloadsPriority3} =
    lists:partition(
      fun({_Key, Priority, _Payload}) -> Priority =< 2 end,
      OddKeysAndPrioritiesAndPayloads
     ),
    KeysAndPrioritiesAndPayloads2 = OddKeysAndPrioritiesAndPayloadsPriorities1And2
    ++ ReAddedKeysAndPrioritiesAndPayloads ++ OddKeysAndPrioritiesAndPayloadsPriority3,
    Queue4 = lists:foldl(
               fun({Key, Priority, Payload}, CurrentQueue) ->
                       {ok, NewQueue} = querl_priority_queue:in(CurrentQueue, Key, Priority, Payload),

                       NewQueue
               end,
               Queue3,
               ReAddedKeysAndPrioritiesAndPayloads
              ),
    assert_queue_equals(KeysAndPrioritiesAndPayloads2, Queue4),

    %% last but not least, let's update the priorities of 3 random items:
    %% 11 is set to priority 3, 28 is set to priority 1, 39 is set to priority 2
    NewPriorities = [{28, 1, 2}, {39, 2, 3}, {11, 3, 1}],
    KeysAndPrioritiesAndPayloads3ByPriorities =
    lists:foldl(
      fun({Key, Priority, _Payload} = Item, CurrentKeysAndPrioritiesAndPayloads3ByPriorities) ->
              case lists:keyfind(Key, 1, NewPriorities) of
                  false ->
                      maps:update_with(
                        Priority,
                        fun(List) -> [Item | List] end,
                        [Item],
                        CurrentKeysAndPrioritiesAndPayloads3ByPriorities
                       );
                  _Else ->
                      CurrentKeysAndPrioritiesAndPayloads3ByPriorities
              end
      end,
      #{},
      lists:reverse(KeysAndPrioritiesAndPayloads2)
     ),
    KeysAndPrioritiesAndPayloads3 = lists:foldl(
                                      fun({Key, NewPriority, _OldPriority}, CurrentKeysAndPrioritiesAndPayloads3) ->
                                              CurrentKeysAndPrioritiesAndPayloads3
                                              ++ maps:get(NewPriority, KeysAndPrioritiesAndPayloads3ByPriorities)
                                              ++ [{Key, NewPriority, Key * 2}]
                                      end,
                                      [],
                                      NewPriorities
                                     ),
    Queue5 = lists:foldl(
               fun({Key, NewPriority, OldPriority}, CurrentQueue) ->
                       {ok, NewQueue, OldPriority} =
                       querl_priority_queue:update_priority(CurrentQueue, Key, NewPriority),

                       NewQueue
               end,
               querl_priority_queue:clone(Queue4),
               NewPriorities
              ),
    assert_queue_equals(KeysAndPrioritiesAndPayloads3, Queue5),

    %% trying to update the priority for a key that doesn't exist yields an
    %% explicit error
    ?assertEqual({error, not_found},
                 querl_priority_queue:update_priority(Queue5, -1, 3)),
    %% and trying to update the priority to the current priority leaves the
    %% queue unchanged
    ?assertEqual({ok, Queue5, 1},
                 querl_priority_queue:update_priority(Queue5, 28, 1)),

    %% and we can do the exact same with `update_priority/4'
    Queue5Bis = lists:foldl(
                  fun({Key, NewPriority, OldPriority}, CurrentQueue) ->
                          {ok, NewQueue} =
                          querl_priority_queue:update_priority(CurrentQueue, Key, OldPriority, NewPriority),

                          NewQueue
                  end,
                  querl_priority_queue:clone(Queue4),
                  NewPriorities
                 ),
    assert_queue_equals(KeysAndPrioritiesAndPayloads3, Queue5Bis),

    ?assertEqual({error, not_found},
                 querl_priority_queue:update_priority(Queue5Bis, -1, 3, 3)),
    ?assertEqual({ok, Queue5Bis},
                 querl_priority_queue:update_priority(Queue5Bis, 28, 1, 1)).

%%% Private helpers

%% @private
assert_queue_equals(ExpectedKeysAndPrioritiesAndPayloads, Queue) ->
    [{_LastKey, ExpectedLowestPriority, _LastPayload} | _] = lists:reverse(ExpectedKeysAndPrioritiesAndPayloads),
    ActualLowestPriority = querl_priority_queue:lowest_priority(Queue),

    ?assertEqual(ExpectedLowestPriority, ActualLowestPriority),

    %% test every priority
    lists:foreach(
      fun(FromPriority) ->
              assert_priorities_equal_from(ExpectedKeysAndPrioritiesAndPayloads, Queue, FromPriority)
      end,
      lists:seq(1, ActualLowestPriority)
     ),

    %% now let's try `out'-ing all the possible sublengths for all priorities
    assert_priorities_equal_from_and_to(ExpectedKeysAndPrioritiesAndPayloads, Queue, all_priorities).

%% @private
assert_priorities_equal_from(ExpectedKeysAndPrioritiesAndPayloads, Queue, FromPriority) ->
    lists:foreach(
      fun(ToPriority) ->
              RelevantExpectedKeysAndPrioritiesAndPayloads =
              lists:filter(
                fun({_Key, Priority, _Payload}) ->
                        Priority >= FromPriority andalso Priority < ToPriority
                end,
                ExpectedKeysAndPrioritiesAndPayloads
               ),

              assert_priorities_equal_from_and_to(RelevantExpectedKeysAndPrioritiesAndPayloads, Queue, {FromPriority, ToPriority})
      end,
      lists:seq(FromPriority, querl_priority_queue:lowest_priority(Queue) + 1)
     ).

%% @private
assert_priorities_equal_from_and_to(ExpectedKeysAndPrioritiesAndPayloads, Queue, FromAndToPriority) ->
    assert_priorities_equal_from_and_to_without_sublists(ExpectedKeysAndPrioritiesAndPayloads, Queue, FromAndToPriority),

    ExpectedSize = erlang:length(ExpectedKeysAndPrioritiesAndPayloads),

    %% now let's try `out'-ing all the possible sublengths
    lists:foreach(
      fun(RequestedSize) ->
              {ExpectedKeysAndPrioritiesAndPayloadsFromOut, ExpectedRemainingKeysAndPrioritiesAndPayloads} =
              lists:split(RequestedSize, ExpectedKeysAndPrioritiesAndPayloads),

              {QueueOut, ActualSize, ActualKeysAndPrioritiesAndPayloadsFromOut} =
              apply_queue_callback_from_and_to(out, Queue, [RequestedSize], FromAndToPriority),

              ?assertEqual(RequestedSize, ActualSize),
              ?assertEqual(ExpectedKeysAndPrioritiesAndPayloadsFromOut, ActualKeysAndPrioritiesAndPayloadsFromOut),

              assert_priorities_equal_from_and_to_without_sublists(ExpectedRemainingKeysAndPrioritiesAndPayloads, QueueOut, FromAndToPriority)
      end,
      lists:seq(0, ExpectedSize)
     ),

    %% and finally let's try `out'-ing more than the size
    {RemainingQueue, ActualTotalSize, ActualKeysAndPrioritiesAndPayloads} =
    apply_queue_callback_from_and_to(out, Queue, [ExpectedSize + 1], FromAndToPriority),

    ?assertEqual(ExpectedSize, ActualTotalSize),
    ?assertEqual(ExpectedKeysAndPrioritiesAndPayloads, ActualKeysAndPrioritiesAndPayloads),

    maybe_assert_queue_empty(FromAndToPriority, RemainingQueue).

%% @private
assert_priorities_equal_from_and_to_without_sublists(ExpectedKeysAndPrioritiesAndPayloads, Queue, FromAndToPriority) ->
    ExpectedSize = erlang:length(ExpectedKeysAndPrioritiesAndPayloads),

    {RemainingQueue, ActualSize1, ActualKeysAndPrioritiesAndPayloads} =
    apply_queue_callback_from_and_to(empty, Queue, [], FromAndToPriority),
    ActualSize2 = compute_actual_size_from_and_to(Queue, FromAndToPriority),

    ?assertEqual(ExpectedSize, ActualSize1),
    ?assertEqual(ExpectedSize, ActualSize2),

    ?assertEqual(ExpectedKeysAndPrioritiesAndPayloads, ActualKeysAndPrioritiesAndPayloads),

    maybe_assert_queue_empty(FromAndToPriority, RemainingQueue).

%% @private
compute_actual_size_from_and_to(Queue, all_priorities) ->
    querl_priority_queue:size(Queue);
compute_actual_size_from_and_to(Queue, {FromPriority, ToPriority}) ->
    lists:foldl(
      fun(Priority, Sum) ->
              Sum + querl_priority_queue:size(Queue, Priority)
      end,
      0,
      lists:seq(FromPriority, ToPriority - 1)
     ).

%% @private
maybe_assert_queue_empty(all_priorities, RemainingQueue) -> assert_queue_empty(RemainingQueue);
maybe_assert_queue_empty(_Else, _RemainingQueue) -> ok.

%% @private
assert_queue_empty(Queue) ->
    ?assertMatch({_, 0, []}, querl_priority_queue:empty(Queue)),
    ?assertEqual(0, querl_priority_queue:size(Queue)).

%% @private
apply_queue_callback_from_and_to(CallbackName, Queue, OtherArgs, FromAndToPriority) ->
    Args = case FromAndToPriority of
               all_priorities -> OtherArgs;
               {FromPriority, ToPriority} -> OtherArgs ++ [FromPriority, ToPriority]
           end,
    Clone = querl_priority_queue:clone(Queue),
    erlang:apply(querl_priority_queue, CallbackName, [Clone | Args]).
