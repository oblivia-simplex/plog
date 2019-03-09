# Prolog-markdown

Markdown parser implemented in Prolog. Compatible with [SWI-Prolog](http://www.swi-prolog.org/) as the
output tree is for direct use by [html/1](http://www.swi-prolog.org/pldoc/doc_for?object=html/1).
The specification for the parser was taken from
<http://daringfireball.net/projects/markdown/syntax> (Gruber's Markdown).

## Example usage

Parse into a structure usable by
[html/1](http://www.swi-prolog.org/pldoc/doc_for?object=html/1).

    :- use_module(library(md/md_parse)).

    ?- md_parse_string("# Hello #", Blocks).
    Blocks = [h1("Hello")].

Convert into an HTML string:

    :- use_module(library(md/md_parse)).

    ?- md_html_string("# Hello #", Html).
    Html = "<h1>Hello</h1>".

## Deviations from the Gruber's Markdown

 * Some cases for tight markup (no separate lines between blocks).
 * No clever encoding for mail addresses.
 * Line break rule creates `<br>` not `<br />`.
 * No in-word emphasis with underscores.
 * Strikethrough as `~~text~~`.
 * Added escape sequences `\~` and `` \` ``.
 * Github-styled fenced code blocks (<https://help.github.com/articles/github-flavored-markdown>).
   No syntax highlighting is provided but the `data-language` attribute is set.
 * Plain link recognizion.

## Performance

Example [document](http://daringfireball.net/projects/markdown/syntax.text) (about 800 lines) is parsed
in 80ms on 2.4GHz Q6600.

## Installation

Requires SWI-Prolog 7.x.

    pack_install('http://packs.rlaanemets.com/markdown/markdown-*.tgz')

## API documentation

See <http://packs.rlaanemets.com/markdown/doc/md_parse.html> for the top-level module documentation.

## Changelog

 * 2014-02-26 version 0.0.2. Fixed HTTPS link in angled brackets. Trimmed bottom of code block.
 * 2014-01-14 version 0.0.1

## Bug reports/feature requests

Please send bug reports/feature request through the GitHub
project [page](https://github.com/rla/prolog-markdown).

## License

The MIT License. See the LICENSE file.
