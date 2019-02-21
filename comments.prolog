:- module(comments, [test_form_page_handler/1]).
/***

What I'd like to do, here, is

* provide the user with a form at the bottom of each post, where they
  can submit a comment of N characters or less
* save that comment as a file, under the directory
  /content/unscreened_comments/${POST_IDENTIFIER}/${TIMESTAMP}-${USERID}.txt
* have another directory for screened comments
  /content/screened_comments/${POST_IDENTIFIER}/${TIMESTAMP}-${USERID}.txt
* render those comments at the bottom of each post, or maybe in a page linkable
  from each post

***/


:- use_module('lib/html_form').

:- http_handler(root(testform), test_form_page_handler, [id(testform)]).

%%% Lifted from Anniepoo's comments.

http:location(css, '/content/css', []).

test_form_page_handler(Request) :-
	validated_form(
	    reply_html_page(
		      [
              title('Form Page'),
              \html_requires(css('stylesheet.css'))
          ],
		\(comments:test_form_content(Request))),
	reply_html_page(
		  [title('Thanks')],
		  \(comments:test_landing_page_content(Request)))).

test_form_content(_Request) -->
	{
	 debug(html_form, 'in test_form_content~n', [])
	},
     html([
      p('Leave a comment!'),
      style(['.oops {    color: #F00; } ']),
      form([action='/testform', method='POST'], [
	       p([
	          label([for=name],'Name:'),
	          \error_message([for=name], p([class=oops],
					  'You need to type your name in here')),
	          \form_field(Request, length_input_minmax(3, '>'),
			                  input([name=name, type=textarea], []))]),
	       p([
	                label([for=comment], 'Comment (0x1000 character maximum):'),
	                \error_message([for=comment],
                                 p([class=oops],
                                   'Comments must be less than 0x1000 characters.'))]),
         p([
	                \form_field(Request, length_input_minmax(0x1000, '<'),
			                        textarea([name=comment, rows=15, columns=100], []))]),
	       p([
	                input([type=submit, name=submit, value='Submit Comment'], [])])])]).

test_landing_page_content(Request) -->
    {print_term(Request, [output(user_error)])},
	  html([
	              p('Well, that worked')
	          ]).

