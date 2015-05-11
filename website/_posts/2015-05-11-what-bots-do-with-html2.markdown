---
layout: post
title:  What do bots do with HTTP/2 (part 1)
date:   2015-05-11 20:34:00
categories: 
    - http2-general
author: Alcides Viamontes
permalink: /what-do-bots-do-with-http2-1/
---

Of course nobody out there is as crazy as to have a website 
running HTTP/2 only and still care about traffic. 
But that's exactly what you need to monitor protocol adoption 
in different scenarios.
For example, while HTTP/2 already works for a lot of users, 
web bots may have left a long way before catching up.
Even important ones.

Google Webmaster Tools doesn't support HTTP/2-only sites
--------------------------------------------------------


Of the bots out there, perhaps the most important is Google's. 
It is difficult to guess what is his particular behavior.
Maybe  we can use 
Google Webmaster tools as a proxy.
It's the same company after all!

{% image bots_dont_like_http2.png %}

Oops, it seems that somebody is not going anywhere near HTTP/2.
That, or my implementation is too shaky for that bot -- you are welcome 
to argue this point using my Twitter handle! --
So, for the moment at least you may want to play it safe and
have an HTTP/1.1 version of your website available. 
Not that I'm following that advice right now ;-). 
