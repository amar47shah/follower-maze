Unhandled issues:

* What if a client connects with a user id already in use?
* What if a client connects with a user id that is malformed?
* What if a client disconnects before the events are all consumed?
* What if event is malformed?
  * Event has < 2 or > 4 pipe-delimited fields
  * Second field does not match "F", "U", "B", "P", or "S"
  * First, third, or fourth field cannot be read as Integer
* What if a event is duplicated?
* What if two (different) events have the same sequence number?

Improvements:

* Error handling
  * handle common scenarios, like those above
  * release resources, such as connection handles

* Memory Profiling and Optimization
  * investigate whether strict data types would lower memory usage

* Multiple channels, could not be implemented because of sequencing.
  * a direct channel for each client
  * one broadcast channel shared among clients
  * one broadcast channel shared among each client's followers
