[![Build Status](https://travis-ci.org/wk8/querl.svg?branch=master)](https://travis-ci.org/wk8/querl)

# querl

`querl` provides two queue-like data structures that allow extra operations, namely removing and updating elements sitting in the queue.

It does so with optimal complexity, using a native C extension ([a.k.a. a NIF](http://erlang.org/doc/tutorial/nif.html)) for efficiency.

## `querl_queue`

Items in a `querl_queue` each consist of a key and a payload, which can both be totally arbitrary erlang terms. Keys uniquely identify items in a `querl_queue`, and allow removing or updating the payloads associated with them.

Example usage:
```erl
%% create a queue with some stuff in it
Queue1 = querl_queue:new(),
{ok, Queue2} = querl_queue:in(Queue1, key1, <<"payload1">>),
{ok, Queue3} = querl_queue:in(Queue2, "key2", "payload2"),
{ok, Queue4} = querl_queue:in(Queue3, <<"key3">>, payload3),
{ok, Queue5} = querl_queue:in(Queue4, <<"key4">>, payload4),
querl_queue:to_list(Queue5).

%% [{key1,<<"payload1">>},
%%  {"key2","payload2"},
%%  {<<"key3">>,payload3},
%%  {<<"key4">>,payload4}]

{ok, Queue6, PreviousPayload3} = querl_queue:remove(Queue5, <<"key3">>),
PreviousPayload3.

%% payload3

querl_queue:to_list(Queue6).

%% [{key1,<<"payload1">>},
%%  {"key2","payload2"},
%%  {<<"key4">>,payload4}]

{ok, Queue7, PreviousPayload2} = querl_queue:update(Queue6, "key2", "new_payload2"),
PreviousPayload2.

%% "payload2"

querl_queue:to_list(Queue7).

%% [{key1,<<"payload1">>},
%%  {"key2","new_payload2"},
%%  {<<"key4">>,payload4}]

{ok, Queue8, PreviousPayload1} = querl_queue:update(Queue7, key1, fun(Bin) -> <<"new_", Bin/binary>> end),
PreviousPayload1.

%% <<"payload1">>

querl_queue:to_list(Queue8).

%% [{key1,<<"new_payload1">>},
%%  {"key2","new_payload2"},
%%  {<<"key4">>,payload4}]

{Queue9, not_present_before} = querl_queue:in_or_update(Queue8, key5, fun(_X) -> erlang:error(wont_be_called_since_key_not_present_before) end, payload5),
querl_queue:to_list(Queue9).

%% [{key1,<<"new_payload1">>},
%%  {"key2","new_payload2"},
%%  {<<"key4">>,payload4},
%%  {key5,payload5}]

{Queue10, 2, KeysAndPayloads} = querl_queue:out(Queue9, 2),
KeysAndPayloads.

%% [{key1,<<"new_payload1">>},{"key2","new_payload2"}]

querl_queue:to_list(Queue10).

%% [{<<"key4">>,payload4},{key5,payload5}]
```

All operations have optimal complexity, more specifically:

|  operation   |                  complexity                   |
|:------------:|:---------------------------------------------:|
| in           | O(1)                                          |
| out          | O(k) where k is the number of requested items |
| empty        | O(n)                                          |
| remove       | O(1)                                          |
| update       | O(1)                                          |
| in_or_update | O(1)                                          |
| fold         | O(n)                                          |
| size         | O(1)                                          |
| to_list      | O(n)                                          |

`querl_queue` exports two types:
```erl
-type querl_queue:queue(Key, Payload).
-type querl_queue:queue() :: querl_queue:queue(any(), any()).
```

[See the exhaustive edown-generated doc here](https://htmlpreview.github.io/?https://raw.githubusercontent.com/wk8/querl/master/doc/querl_queue.html).

## `querl_priority_queue`

`querl_priority_queue`s have the same notion of keys and payloads as `querl_queue`s, with the addition of priorities, which are integers in the range 1 to `C` inclusive, where 1 is the highest priority, and `C` is the lowest priority (which must be passed when creating a new `querl_priority_queue`).

See [the tests](https://github.com/wk8/querl/blob/master/test/querl_priority_queue_tests.erl) for some examples.

All operations have optimal complexity, more specifically:

|     operation      |                    complexity                     |
|:------------------:|:-------------------------------------------------:|
| in                 | O(1)                                              |
| out                | O(k + C) where k is the number of requested items |
| empty              | O(n + C)                                          |
| remove             | O(1)                                              |
| update_priority    | O(1)                                              |
| size               | O(1)                                              |
| lowest_priority    | O(1)                                              |
| to_list            | O(n)                                              |

Same as `querl_queue`, `querl_priority_queue` exports two types:
```erl
-type querl_priority_queue:queue(Key, Payload).
-type querl_priority_queue:queue() :: querl_priority_queue:queue(any(), any()).
```

[See the exhaustive edown-generated doc here](https://htmlpreview.github.io/?https://raw.githubusercontent.com/wk8/querl/master/doc/querl_priority_queue.html).

## Caveats

### Not thread-safe

These data structures are _not_ thread safe, that is you cannot use the same `querl_queue` or `querl_priority_queue` from two (or more) Erlang processes at the same time.

### Destructive operations

Contrary to what you're used to in Erlang, variables containing a `querl_queue` or a `querl_priority_queue` are not immutable. In particular, any operation that changes the state of the queue effectively invalidate any variable containing previous versions of the data, and trying to use them in any way will result in a `badarg` exception, for example:

```erl
Queue1 = querl_queue:new(),
{ok, Queue2} = querl_queue:in(Queue1, key1, <<"payload1">>),
querl_queue:to_list(Queue2).

%% [{key1,<<"payload1">>}]

{ok, Queue3} = querl_queue:in(Queue2, "key2", "payload2"),
querl_queue:to_list(Queue3).

%% [{key1,<<"payload1">>},{"key2","payload2"}]

%% at this point, both `Queue1` and `Queue2` have been invalidated, and trying to use them in any will result in `badarg` exceptions:

querl_queue:to_list(Queue1).
** exception error: bad argument
querl_queue:in(Queue2, "key2", "payload2").
** exception error: bad argument
```

In a nutshell, just make sure to always the latest queue variable you have in subsequent calls.

All destructive operations are clearly marked as such in the doc for both data structures.
