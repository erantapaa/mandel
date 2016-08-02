
This is a reply to the SO question: http://stackoverflow.com/questions/38705886

Note: These comments refer to the modified version of code
where the min/max iteration length is set to 10 and 20 respectively.

The basic problem is that the worker thread is hogging
the TQueue and depriving the manager thread an opportunity
from removing items from the queue. Note that the
STM transaction performed by the worker is very cheap - it is
just adding some elements to the queue. The manager, on the
other hand, is trying to remove _all_ of the items from the
queue. This makes the manager transaction much more likely to
fail.

Also, even though the manager is only interested in writing out
1000 Traces, the worker is in a forever loop and keeps generating
Traces even after this goal is reached. Adding some debugging statements
shows that the worker typically adds Traces at a rate in excess
of of 100K per second. As the queue gets longer and longer the
chance that the manager transaction will succeed diminishes rapidly.

There are several way to modify your existing code to mitigate
these issues:

- have the worker thread stop after mdNumTraces Traces have been generated
  
- have the worker thread sleep periodically (like after adding 100 Traces); even a delay of a microsecond can be enough

Ultimately I think the best course of action is to use a better
concurrent queue. The one you've built with STM allows writers
to deny access to a reader which clearly is very undesirable.

### Examples

__Lib.hs__ - `min_it` and `max_it` set to 10 / 20. Added debugging. Manager only write 1000 traces. Worker thread emits stats every 1000 iterations.

__Lib2.hs__ - `min_it` / `max_it` set to 10 / 20.  Manager only writes 1000 traces. 

__Lib3.hs__ - Rewritten to use the [unagi-chan][unagi-chan] library. `min_it` / `max_it` unchanged from original code (1000 / 10000). Manager does not explicitly sleep.

  [unagi-chan]: https://hackage.haskell.org/package/unagi-chan

