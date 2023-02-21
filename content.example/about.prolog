:- module(about, [title/1,
                  admin/1,
                  domain/1,
                  email/1,
                  port/1,
                  motto/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Put your blog's information here %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

title('Your Title Goes Here').
admin('Your Name Goes Here').
domain('your.domain.name').
email('your@email.com').
motto('mottos.txt').
repo('https://github.com/your-github-username/your-repo-name').
port(9697). 
bind('0.0.0.0').
timezone(4). % look up your timezone and put it here
