Unhandled issues:

* What if a client connects with a user id already in use?
* What if a client connects with a user id that is malformed?
* What if a client disconnects before the events are all consumed?
* What if messages are malformed?

Improvements:

* Multiple channels, could not be implemented because of sequencing.
  * a direct channel for each client
  * one broadcast channel shared among clients
  * one broadcast channel shared among each client's followers
