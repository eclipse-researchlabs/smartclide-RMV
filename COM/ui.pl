% USER INTERFACE
:- module(ui,[select_mult_list/3,select_one_list/3,
	      display_list/1, display_listq/1, display_list/2, display_listq/2,
	      timestamp2text/2, timestamp2text/3, text2timestamp/2,
	      display_datetime/1, display_timestamp_datetime/1, display_timestamp_datetime/2,
	      enter_a_number/2,
	      gdisplay_term/1,
	      notify/2]).

% :- use_module(library(gvterm)).

% :- use_module(library(graphml_ugraph)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% User Interface
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Primitive actions
%
%   select_mult_list
%   select_one_list
%   display_list
%   enter_a_number
%   display a date-time

select_mult_list(List,Mark,Selection) :-
	writeln('Select one or more from;'),
	display_list(List,Mark),
	write('Enter selection as list [...]. : '), flush_output,
	read(Selection).

select_one_list(List,Mark,Selection) :-
	writeln('Select one from;'),
	display_list(List,Mark),
	write('Enter selection as Item #. : '), flush_output,
	read(Selection).

gdisplay_term(_L) :-
	% dotty_term(L),
	true.

display_list([]) :- flush_output.
display_list([T|L]) :-
	writeln(T),
	display_list(L).

display_list([],_) :- flush_output.
display_list([Item|Items],N) :- number(N), !,
	format('~t~d)~4| ', N), % keep column up to 999
	write(Item), nl,
	N1 is N + 1,
	display_list(Items,N1).
display_list([Item|Items],Mark) :- atom(Mark), !,
	format(' ~a ',Mark),
	write(Item), nl,
	display_list(Items,Mark).

display_listq([]) :- flush_output.
display_listq([T|L]) :-
	writeq(T), nl,
	display_listq(L).

display_listq([],_).
display_listq([Item|Items],N) :- number(N), !,
	format('~t~d)~4| ', N), % keep column up to 999
	writeq(Item), nl,
	N1 is N + 1,
	display_listq(Items,N1).
display_listq([Item|Items],Mark) :- atom(Mark), !,
	format(' ~a ',Mark),
	writeq(Item), nl,
	display_listq(Items,Mark).

% conversions to/from readable time, and display time

timestamp2text(TS,Text) :-
	stamp_date_time(TS,DateTime,local),
	format_time(atom(Text),'%F %T%Z',DateTime).

timestamp2text(TS,Text,rfc3339) :- !,
	stamp_date_time(TS,DateTime,local),
	format_time(atom(Text),'%FT%T%z',DateTime).

timestamp2text(TS,Text,rfc3339z) :- !,
	stamp_date_time(TS,DateTime,'UTC'),
	format_time(atom(Text),'%FT%TZ',DateTime).

text2timestamp(Text,TS) :- parse_time(Text,iso_8601,TS).

display_datetime(DateTime) :-
	writeq(DateTime), nl.

display_timestamp_datetime(TS) :-
	stamp_date_time(TS,DateTime,'UTC'),
	format_time(atom(Text),'%F %T %Z',DateTime),
	display_datetime(Text).

display_timestamp_datetime(TS,rfc3339) :-
	stamp_date_time(TS,DateTime,'UTC'),
	format_time(atom(Text),'%FT%TZ',DateTime),
	display_datetime(Text).

% -------------------------------------------------

portray(T) :- writeq(T).

enter_a_number(N,Message) :-
	format('~a~nEnter a number...~n',Message),
	readln(L),
	(   L=[N], number(N)
	->  true
	;   enter_a_number(N,Message)
	).

notify(warning,M) :- !,
	(   atom(M)
	->  format('Warning: ~a~n',[M]), flush_output
	;   true
	).
notify(notice,M) :- !,
	(   atom(M)
	->  format('Notice: ~a~n',[M]), flush_output
	;   true
	).
notify(Kind,M) :- !,
	(   atom(Kind), atom(M)
	->  format('~a: ~a~n',[Kind,M]), flush_output
	;   true
	).
notify(_,_).

