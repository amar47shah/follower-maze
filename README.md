Unhandled issues:

* What if a client connects with a user id already in use?

Future work:

* Error handling
  * handle common scenarios, like those above

* Restricting duplicates
  * filter exact duplicates in the result of dequeueAll
  * determine rule to choose between different events with same sequence number

* Memory Profiling and Optimization
  * investigate whether strict data types would lower memory usage

* Multiple channels, could not be implemented because of sequencing.
  * a direct channel for each client
  * one broadcast channel shared among clients
  * one broadcast channel shared among each client's followers
