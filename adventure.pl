/* TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek. */

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

# people:
#     jurgen
#     hilda
#     hotel_owner # TODO
#     barman # TODO
#     urlich
#     brother # TODO
#     thomas
#     giulia
#     amy
#     old_colleague # TODO
#     zoe
#     jonas
#     promyczek
#     hunter # TODO
#     theodor
# places:
#     room_of_thomas_and_giulia (thomas, giulia, brother)
#     room_of_zoe (zoe)
#     bar (barman, ex, old_colleague)
#     corridor (jurgen, hilda)
#     kitchen (theodor)
#     reception (owner, promyczek, hunter)
#     hotel_entrance (urlich, jonas)
#     poker_room
#     hunters_shaque
# things:
#     watch (room_of_thomas_and_giulia)
#     thomas_journal (room_of_thomas_and_giulia)
#     cigarette (room_of_thomas_and_giulia)
#     cigarette_light (hotel_entrance)
#     bottle_of_chloroform (room_of_zoe)
#     money (owned by character)
#     *all the things that characters have on them*
# facts:
#     murder
#     poker_is_played_here (TODO)
#     murderer_had_a_watch (TODO)
#     TODO
# moves:
#     goto *place*
#     take *thing*
#     talk to *character*
#         ask about *thing*
#         tell about *fact*
#         bet
#         threaten / press
#         (situational) yes / no

i_am_at(someplace).

path(someplace, n, someplace).

at(thing, someplace).

/* These rules describe how to pick up an object. */

take(X) :-
    holding(X),
    write('You''re already holding it!'),
    !, nl.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    assert(holding(X)),
    write('OK.'),
    !, nl.

take(_) :-
    write('I don''t see it here.'),
    nl.


/* These rules describe how to put down an object. */

drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    write('OK.'),
    !, nl.

drop(_) :-
    write('You aren''t holding it!'),
    nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look.

go(_) :-
    write('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
    i_am_at(Place),
    describe(Place),
    nl,
    notice_objects_at(Place),
    nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
    at(X, Place),
    write('There is a '), write(X), write(' here.'), nl,
    fail.

notice_objects_at(_).


/* This rule tells how to die. */

die :-
    finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
    nl,
    write('The game is over. Please enter the "halt." command.'),
    nl.


/* This rule just writes out game instructions. */

instructions :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('start.             -- to start the game.'), nl,
    write('n.  s.  e.  w.     -- to go in that direction.'), nl,
    write('take(Object).      -- to pick up an object.'), nl,
    write('drop(Object).      -- to put down an object.'), nl,
    write('look.              -- to look around you again.'), nl,
    write('instructions.      -- to see this message again.'), nl,
    write('halt.              -- to end the game and quit.'), nl,
    nl.


/* This rule prints out instructions and tells where you are. */

start :-
    instructions,
    look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(someplace) :- write('You are someplace.'), nl.

