
This is a reply to the SO question: http://stackoverflow.com/questions/38705886

The problem is two-fold:

(1) The manager thread doesn't process any
Traces until it has exhausted the queue.

(2) The worker thread can add elements to the queue very, very fast.

This results in a race that the manager thread never wins.

Adding some debugging code shows that the worker can add
items to the queue in excess of 100K Traces per second.
Moreover, even though the manager is only interested in
writing out the first 1000 Traces, the worker doesn't
stop at this limit. So, under certain circumstances,
the manager is never able to exit this loop:

```haskell
purgeTQueue q =
  whileJust (atomically $ tryReadTQueue q)
            (return . id)
```

The simplest way to fix the code is to have the
manager thread use `readTQueue` to read and process just one
item off the queue at a time. This will also block
the manager thread when the queue us empty obviating
the need to the manager thread to periodically sleep.

Additionally, the worker thread should stop adding
Traces once the desired number has been reached.

### Example Code

### Examples

__Lib4.hs__ contains the minimal changes to the original code to get it to work.

__Lib3.hs__ contains a version which uses the concurrent queue provided by the [unagi-chan][unagi-chan] packge.

  [unagi-chan]: https://hackage.haskell.org/package/unagi-chan

