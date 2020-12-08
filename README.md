![Automated Tests](https://github.com/bodo-hugo-barwich/hash-lists/workflows/Automated%20Tests/badge.svg)
[![Build Status](https://travis-ci.com/bodo-hugo-barwich/hash-lists.svg?branch=master)](https://travis-ci.com/bodo-hugo-barwich/hash-lists)

# hash-lists
ObjectPascal Implementation of the Perl Data Structure known as `%hash` inspired by the Documentation at 
https://www.perl.com/pub/2002/10/01/hashes.html/

## Implementation
The Hash Lists are implemented as 2 Dynamic Arrays which are the Bucket and the List of Buckets.\
As Hash Function it uses the sdbm / ndbm Algorithm \
The best Application are those which can be configured by human beings. Thus the Properties
that determine all over Usage Performance are adjustable by the User according to the Use Case.

## Performance
The Performance of the Hash List depends heavily on the `GrowFactor` and `LoadFactor`.\
Adjusted correctly it achieves an Insertion and Lookup Speed like the similiar Structure for the `Rust` Programming Language,
the Rust `std::collections::HashMap`.\
For Insertions it is more than 10 times faster than the `Generics.Collections.TFastHashMap` Class.\
It also beats the speed of its reference the Perl `%hash` on the Insert Operation on larger data sets.

If you expect a fix amount of elements to be inserted. (Perhaps comming from a SQL Query with fix `Limit`)
it is beneficial to set the List size beforehand with the `Capacity` Property

## Documentation
The Documentation of the Data Structures was generated with the `fpdoc` tool and be found at:\
[Project Documentation in TXT format](doc/hash-lists.txt)\
The Class Diagramm kann be found at:\
[Class Diagram for the Unit 'pointerhash'](doc/pointerhash.jpg)


## Motivation
Using the FCL Data Types `TFPObjectHashTable` and `TFPStringHashTable` the performance of the Application dropped drastically.
Inserting 9 Keys in 4 Objects took 20ms while the Perl-Script doing the same task finished the Job already in 15 ms.\
Learning about highly performant Perl Data Structure was the inspiration to try to implement a Library that could achieve the same Tasks in much less time.\
It also implements the missing Features for the Perl `%hash` as to know the Element Count and Iterating over the Data Structure without the need to copy the Keys.

## Disclaimer
The `TPLPointerHashList` will not free the Values stored in it. You need to free them yourself after usage.\
Though the `TPLObjectHashList` does free the `TObject`s stored in it calling the `TObject.Free()` Method.\
The `TPLStringHashList` copies the String Values and frees them on Destruction.
