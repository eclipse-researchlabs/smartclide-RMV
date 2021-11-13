% Sessions
%   an ephemeral alternative to user ID in access queries

:- module(sessions, [is_session/2, init_session/2, end_session/1]).

:- dynamic session/2.

% session(SessionId, UserId)
session(0,0).

is_session(SessionId, UserId) :-
    (   ground(SessionId) ; ground(UserId) ), !,
    session(SessionId, UserId).

init_session(SessionId, UserId) :-
    ground(SessionId), ground(UserId), !,
    \+is_session(SessionId,UserId),
    assertz( session(SessionId,UserId) ).

end_session(SessionId) :-
    ground(SessionId), is_session(SessionId,_), !,
    retractall( session(SessionId,_) ).

