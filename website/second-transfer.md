---
layout: page
title: Second Transfer
permalink: /second-transfer/
linkorder: 1
---

Second Transfer implements enough HTTP/2  to build 
compliant servers.

Practical matters
=================

Conformance
-----------

So far we are checking conformance by using Moto Ishizawa's 
[h2spec](https://github.com/summerwind).

Check posts inside the category "conformance". 
For example, [this is the url of the first one](/second-transfer/conformance-1/).

Links
-----

Frame encoding and decoding is done with 
Kazu Yamamoto's [http2](http://hackage.haskell.org/package/http2) package.

Links concerning the library itself:

* Github: [https://github.com/alcidesv/second-transfer](https://hackage.haskell.org/package/second-transfer)
* Hackage: [https://hackage.haskell.org/package/second-transfer](https://hackage.haskell.org/package/second-transfer)

Please use Github's issue tracker if you wish to report any issues.
The help is much appreciated!

The library

* Is concurrent, meaning that you can use amazing Haskell lightweight threads to 
process the requests. 

* Obeys HTTP/2 flow control aspects.

* And gives you freedom to (ab)use the HTTP/2 protocol in all the ways envisioned 
by the standard. In particular you should be able to process streaming requests 
(long uploads in POST or PUT requests) and to deliver streaming responses. You
should even be able to do both simultaneously. 

Setting up TLS for HTTP/2 correctly is enough of a shore, so I have bundled here the
TLS setup logic. Before you read any further, ATTENTION: enable always the threaded 
ghc runtime in your final programs if you want TLS to work.

Building
--------

Check the [instructions in the package](https://github.com/alcidesv/second-transfer/blob/master/README.md).


Learning
========

There is a 
[basic tutorial](https://hackage.haskell.org/package/second-transfer-0.3.0.4/docs/SecondTransfer.html)
at hackage. 
More coming soon.
