# Least Recently Used Cache

Key/Value cache with a maximum size.  If a new element is added to the cache, which is already at full size, the least recently used element is dropped from the cache.  The major issue with this implementation is that only a single thread can `set` or `get` at a time, greatly limiting concurrency.  This is because even the `get` modifies the list, moving the retrieved node to the head.

# Least Recently Written Cache

Same concept, except only moves node to head of list when written.  So it is possible for a heavily read key to be evicted as writes occur.  But this implementation has the advantage that many threads can `get` the same time, since the operation does not modify the list.  They are only blocked by a `set`, which is still single thread access, but that should be a very small percentage of requests.
