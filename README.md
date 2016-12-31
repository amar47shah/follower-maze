Unhandled issues:

* What if a client connects with a user id already in use?
* What if a client connects with a user id that is malformed?
* What if a client disconnects before the events are all consumed?

Future work:

* Error handling
  * handle common scenarios, like those above
  * release resources, such as connection handles

* Restricting duplicates
  * filter exact duplicates in the result of dequeueAll
  * determine rule to choose between different events with same sequence number

* Memory Profiling and Optimization
  * investigate whether strict data types would lower memory usage

* Multiple channels, could not be implemented because of sequencing.
  * a direct channel for each client
  * one broadcast channel shared among clients
  * one broadcast channel shared among each client's followers
