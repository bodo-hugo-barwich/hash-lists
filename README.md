# hash-lists
ObjectPascal Implementation of the Perl Data Structure known as `%hash` inspired by the Documentation at 
https://www.perl.com/pub/2002/10/01/hashes.html/

# Implementation
The Hash Lists are implemented as 2 Dynamic Arrays which are the Bucket and the List of Buckets.  As Hash Function I decided to use the sdbm / ndbm Algorithm

# Motivation
Using the FCL Data Types`TFPObjectHashTable` and `TFPStringHashTable` the performance of the Application dropped drastically.
Inserting 9 Keys in 4 Objects took 20ms while the Perl-Script doing the same task finished the Job already in 15 ms.  Learning about highly performant Perl Data Structure inspired me to try to implement a Library that could achieve the same Tasks in much less time.  It also implements the missing Features for the Perl `%hash` as to know the Element Count and Iterating over the Data Structure without the need to copy the Keys.
