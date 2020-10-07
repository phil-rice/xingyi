Stream rather than read and transform

Cleanly separate the two types of many to one relationship
* The ones that are actually one to zero or one, and the ones that are reference

Try and avoid boxing:
* The streaming might help
* Can we stream from the database straight to json? If we have to go via 
* Use a typeclass to work out which method to call
*