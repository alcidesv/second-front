
Second-Front: A Haskell library to write experimental HTTP/2 webservers
=======================================================================

This very experimental server only leverages functionality already 
existent on [second-transfer](http://hackage.haskell.org/package/second-transfer). Installation should be straightforward, but please check first the 
requirements of the second-transfer package.

Installation
------------

Use cabal!

Usage
------

Check the file second-front-src/Main.hs for a straightforward example 
of how to create an HTTP/2 file-server.


How experimental is this
------------------------

Very! If you check the sources of second-transfer, you will see a laundry
list of known things to do. Only [your favorite god here] knows how many
more are lurking below the surface. 