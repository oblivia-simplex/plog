# P'log

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

## Getting Started

### Example Files

In order for anything to actually work, however, you need to first
provide the blog with some content. The fastest way to get started
on this is just to
```
cp -rv content.example/ content/
```
And then make whatever changes you see fit. 

### To Run

To serve on port 8000, make sure that that port and the appropriate bind 
address are specified in `about.prolog`, in your blog directory. Then 
either link, copy, or mount your blog directory under `./content`, and 
run `swipl -l plog.pl -g start`.


## Structure 

Blog posts go in `content/posts`, and should be
in [markdown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)
format. Information pages that aren't exactly *posts*, like the GPL 
license document, or the "about this blog" page, go in `content/info`.

Each markdown file should contain a YAML header. Take a look at the posts in 
`content.example/posts` to see the format. 

But basically it should look like this:

```
---
title: "How I Taught Barnacles to Dance"
tags:
- barnacles
- choreography
date: 2023-01-26
abstract: "They said it couldn't be done, but every last one of them was a coward."
author: me
---
```

The CSS files should go in `content/css`. Use the examples given as a template,
and work from there, until things look the way you'd like. 

The `favicon.ico` can be found in `content/img`, which is where any images linked
to from your posts should also be stored. The links should be relative to the
`content` directory.

```
![look at this mindflayer](img/mindflayer.gif)
```

Metadata for your blog should go in `content/about.prolog`. The syntax used
there should be fairly self-explanatory. 

If you would like to proxy to the P'log server through Nginx, you can
add the following lines to your website's entry in `/etc/nginx/sites-available`,
making whatever adjustments you see fit:
```
    location / {
        try_files $uri @upstream;
    }

    # proxy through to the p'log server
    location @upstream {

        # Disables HEAD requests to the site.
        # SWI-Prolog currently does not support
        # head requests very well.

        if ($request_method = HEAD) {
            return 405;
        }
        
        proxy_pass http://127.0.0.1:8000;
        # or whatever port you're using
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Host $http_host;
        proxy_set_header X-Nginx-Proxy true;
        proxy_redirect off;
    }

```

## Git Layout

It's expected that you will be running P'log from the directory into which
you have cloned its git repository, and that the data that makes up the
meat of your blog will live in its own git repo, under `./content/`. This
is strictly necessary at this point, since P'log will expect to retrieve
git information from time to time. Maybe I'll loosen this constraint in the
future.


## Using Docker

If you want to run P'Log in a Docker container, you can do so easily. Just run
`make`, and then use the `plogserve.sh` script, like so:

```
$ ./plogserve.sh $PATH_TO_YOUR_BLOG $BIND_ADDRESS $PORT
```

# Blogs Powered by P'Log

This list is complete. You can help by adding to it. 

1. [Feral Machines](http://feralmachin.es)

