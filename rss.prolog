:- use_module(about).
:- use_module(git).

xml_header([
               '<?xml version="1.0" encoding="utf-8"?>',
               '<rss version="2.0">',
               '<channel>',
               '<title>',
               Title,
               '</title>',
               '<link>',
               Link,
               '</link>',
               '<description>',
               Description,
               '</description>',
               '<lastBuildDate>',
               LastBuildDate,  % format: Wed, July 4, 2018 % GMT!
               '</lastBuildDate>'
              ]) :-
    about(title(Title),
          admin(_),
          domain(Domain),
          abstract(Description)),
    atom_concat('//', Domain, Link_),
    atom_concat(Link_, '/', Link),
    last_build_date_of_repo(LastBuildDate).


