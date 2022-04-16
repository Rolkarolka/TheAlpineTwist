/* TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek. */

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/** people:
*     hotel_owner * TODO name
*     barman * TODO name
*     brother * TODO name
*     old_colleague * TODO name
*     hunter * TODO name
* things:
*     *all the things that characters have on them*
* facts:
*     murder
*     poker_is_played_here (TODO)
*     murderer_had_a_watch (TODO)
*     TODO
* moves:
*     goto *place*
*     take *thing*
*     talk to *character*
*         ask about *thing*
*         tell about *fact*
*         bet
*         threaten / press
*         (situational) yes / no
*/



/* --- DEFINITIONS OF PEOPLE, THINGS AND PLACES --- */



/*  These rules defines connections between places */
path(room_of_thomas_and_giulia, s, corridor).

path(corridor, n, room_of_thomas_and_giulia).
path(corridor, s, room_of_zoe).
path(corridor, e, reception).

path(room_of_zoe, n, corridor).

path(reception, w, corridor).
path(reception, s, bar).
path(reception, e, hotel_entrance).
path(reception, n, kitchen).

path(bar, e, reception).
path(bar, w, kitchen).
path(bar, s, poker_room).

path(hotel_entrance, w, reception).
path(hotel_entrance, s, hunters_shaque).

path(kitchen, s, reception).
path(kitchen, e, bar).
path(kitchen, w, hunters_shaque).

path(hunters_shaque, n, hotel_entrance).
path(hunters_shaque, e, kitchen).

path(poker_room, n, bar).

/* These rules describe where everything and everyone is. */

i_am_at(room_of_thomas_and_giulia).
at(thomas, room_of_thomas_and_giulia).
at(giulia, room_of_thomas_and_giulia).
at(brother, room_of_thomas_and_giulia).
at(zoe, room_of_zoe).
at(barman, bar).
at(ex, bar).
at(old_colleague, bar).
at(jurgen, corridor).
at(hilda, corridor).
at(theodor, kitchen).
at(owner, reception).
at(hunter, reception).
at(promyczek, reception).
at(jonas, hotel_entrance).
at(urlich, hotel_entrance).

at(watch, room_of_thomas_and_giulia).
at(thomas_journal, room_of_thomas_and_giulia).
at(cigarette, room_of_thomas_and_giulia).
at(cigarette_light, hotel_entrance).
at(bottle_of_chloroform, room_of_zoe).
holding(money).


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(room_of_thomas_and_giulia) :- write('You are in room of Thomas and Giulia.'), nl.
describe(room_of_zoe) :- write('You are in room of Zoe.'), nl.
describe(bar) :- write('You are in the bar.'), nl.
describe(corridor) :- write('You are in the corridor.'), nl.
describe(kitchen) :- write('You are in the kitchen.'), nl.
describe(reception) :- write('You are in the reception.'), nl.
describe(hotel_entrance) :- write('You are at the hotels entrance.'), nl.
describe(poker_room) :- write('You are in the secret poker room.'), nl.
describe(hunters_shaque) :- write('You are in the hunter\'s shaque.'), nl.



/* --- DEFINITIONS OF RULES --- */



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


/* This rule tells how to move in a given direction. */

go(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look.

go(_) :-
    write('You can''t go that way.').


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to look around. */

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
    write('There is '), write(X), write(' here.'), nl,
    fail.

notice_objects_at(_).


/* This rules define how to talk to someon */

talk_to(Person) :-
    i_am_at(Place),
    at(Person, Place),
    write("You start talking to "), write(Person), write('.'),
    !.

talk_to(Person) :-
    write('You start to formulate your sentence towards '), write(Person), write(', when suddenly you realise, that he cannot hear you, for he isn\'t here.'),
    nl.


/* --- REST OF DEFINITIONS --- */



/* This rule tells how to die. */

die :-
    finish.

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
    write('look.              -- to look around you again.'), nl,
    write('instructions.      -- to see this message again.'), nl,
    write('halt.              -- to end the game and quit.'), nl,
    nl.


/* This rule prints out instructions and tells where you are. */

start :-
    instructions,
    look.
