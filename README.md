# Follower Maze

The challenge solved here is to build a system which acts as a socket
server, reading events from an *event source* and forwarding them when
appropriate to *user clients*.

Clients connect through TCP and use the simple protocol described in a
section below. There are two types of clients connecting to the server:

- **One** *event source*: This sends a
stream of events which may or may not require clients to be notified
- **Many** *user clients*: Each one representing a specific user,
these wait for notifications for the events that are relevant to the
user they represent

Included in the `instructions` folder is a jar file and a shell script.
These contain one possible implementation of the *event source* and *user client*.

A full specification is given in the `instructions/instructions.md`.

## Setup

1. Install [stack](http://docs.haskellstack.org/en/stable/README.html).
2. `cd` into the project.
3. `stack setup`

## Usage

In one console, run this project:

    stack build && stack exec follower-maze-exe

In another console, run the provided test input script:

    cd instructions
    ./followermaze.sh

With special configurations:

    cd instructions
    totalEvents=1000 concurrencyLevel=200 ./followermaze.sh

For more information, see the `instructions/instructions.md`

## Dependencies

Unlike other standard libraries, GHC's minimalist `base` does not include
modules for set and dictionary data structures, safe string-to-integer
conversion, and simple socket handling. Therefore, this project depends
on the `containers`, `safe`, and `network` packages.

This project depends on the `stm` package, which provides composably
atomic transactions in which to operate on shared state from multiple
threads (software transactional memory). Software transactional memory
can be used to scale atomicity without explicitly managing threaded
resource access. Each transaction can be treated as a single-threaded
computation, resulting in programs that are more modular and less
error-prone than those using locking.

## Documentation

Generate and open Haddock with `stack haddock follower-maze --open`

## Testing

Run the test suite with `stack test`

Generative tests are provided, using QuickCheck, to test invariant
properties of the event parsing and queueing behavior implemented in
`FollowerMaze.Event` and `FollowerMaze.EventQueue`. Additionally,
several representative unit test cases are included to verify that
bad event inputs are not parsed.

## Stress Testing

The following running times were obtained by varying one configuration
parameter while maintaining the other defaults:

```
|             totalEvents |   1e7 (default) |   5e7 |   1e8 |
|-------------------------|-----------------|-------|-------|
|    succeeded in (mm:ss) |            5:38 | 29:32 | 57:37 |
```

```
|           numberOfUsers |  1000 (default) |  5000 |  8000 | 10000 |
|-------------------------|-----------------|-------|-------|-------|
|    succeeded in (mm:ss) |            5:38 | 24:51 | 46:51 | 62:18 |
```

```
|        concurrencyLevel |   100 (default) |   200 |   500 |   650 |   750 |    800 |   1000 |
|-------------------------|-----------------|-------|-------|-------|-------|--------|--------|
|    succeeded in (mm:ss) |            5:38 |  6:18 |  9:39 |  9:26 | 10:06 | failed | failed |
```

```
| maxEventSourceBatchSize |   100 (default) |   500 |  1000 |  5000 | 10000 |
|-------------------------|-----------------|-------|-------|-------|-------|
|    succeeded in (mm:ss) |            5:38 |  5:39 |  5:17 |  5:06 |  5:16 |
```

The following running times were obtained by varying `numberOfUsers` and
`concurrencyLevel` in tandem:

```
|                       concurrencyLevel |
|                                        |
|                       |   100 |   1000 |
|                 ------|-------|--------|
|                  1000 |  5:38 | failed |
| numberOfUsers   ------|-------|--------|
|                 10000 | 62:18 | 204:16 |
```

## Future work

* Profiling and Optimization
  * measure CPU performance
  * measure memory performance
  * use GHC optimization flags

* Handle potential errors in the event stream
  * filter exact duplicates in the result of `FollowerMaze.EventQueue.dequeueAll`
  * determine rule to choose between different events with same sequence number
  * determine rule to accommodate a missing sequence number

* Use multiple channels
  * one chan special to each client, to receive private messages and follow notifications
  * one server broadcast chan, duplicated to all clients, to receive broadcasts
  * one broadcast chan for each client, duplicated to all followers, to receive status updates
  * advantage: avoids building a costly STM transaction for broadcasts and status updates
  * disadvantage: more complex logic needed to ensure order of messages from multiple chans
