# README #

What I'm trying to do here is set up a minimalistic blogging
platform, and learn a bit of practically-oriented Prolog in
the process. I'm leaning pretty heavily on 
[Annie Ogborn's tutorial](http://www.pathwayslms.com/swipltuts/html/index.html)
at this point. 

I really dislike doing any composition in the browser, and so
what I'm after here is just a system that will take a directory
of markdown files, and a table of contents file in a nice, simple
syntax of its own, and take care of beautifying and serving the
documents from there. Posting to the blog will just be done through
SSH. I don't see any need to complicate that any further. 

Maybe I'll figure out a neat way of integrating comments, etc., 
in the future. 

## Dependencies
[Raivo Laanemets' Markdown Pack, for SWI Prolog](http://packs.rlaanemets.com/markdown/markdown-0.0.2.tgz)

Install this with
```
:- pack_install('http://packs.rlaanemets.com/markdown/markdown-0.0.2.tgz').
```
from your `swipl` REPL.

## To Run
To serve on port 8000, run:
```
$ swipl plog.prolog

:- server(8000). 
```

## Example Files
Before using, the `*.example` files should, at the very least, be copied
to filenames where the `.example` suffix is dropped.
