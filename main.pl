/* TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek. */

:- dynamic i_am_at/1, person_at/2, thing_at/2, holding/1, talking_to/1, i_know/1.
:- retractall(person_at(_, _)), retractall(thing_at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(i_know(_)).

/** facts:
*     thomas_had_been_murdered
*     poker_is_played_here
*     murderer_had_a_watch
*     watch_has_changed_hands_during_last_game
*     zoe_befriended_hilda
*     zoe_was_thomas_lovers
*     zoe_knew_about_watch_changing_hands
*     zoe_has_chloroform
*     zoe_knew_about_giulia
*     TODO more
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
person_at(thomas, room_of_thomas_and_giulia).
person_at(giulia, room_of_thomas_and_giulia).
person_at(andreas, room_of_thomas_and_giulia).
person_at(zoe, room_of_zoe).
person_at(karl, bar).
person_at(ex, bar).
person_at(stephan, bar).
person_at(jurgen, corridor).
person_at(hilda, corridor).
person_at(theodor, kitchen).
person_at(hans, reception).
person_at(hermann, reception).
person_at(promyczek, reception).
person_at(jonas, hotel_entrance).
person_at(urlich, hotel_entrance).

thing_at(watch, room_of_thomas_and_giulia).
thing_at(thomas_journal, room_of_thomas_and_giulia).
thing_at(cigarette, room_of_thomas_and_giulia).
thing_at(cigarette_light, hotel_entrance).
thing_at(bottle_of_chloroform, room_of_zoe).

thing_at(broche, hilda).

holding(money).

/* The facts that the player knows about from start */

i_know(thomas_had_been_murdered).

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe_place(room_of_thomas_and_giulia) :- write('You are in room of Thomas and Giulia.'), nl.
describe_place(room_of_zoe) :- write('You are in room of Zoe.'), nl.
describe_place(bar) :- write('You are in the bar.'), nl.
describe_place(corridor) :- write('You are in the corridor.'), nl.
describe_place(kitchen) :- write('You are in the kitchen.'), nl.
describe_place(reception) :- write('You are in the reception.'), nl.
describe_place(hotel_entrance) :- write('You are at the hotels entrance.'), nl.
describe_place(poker_room) :- write('You are in the secret poker room.'), nl.
describe_place(hunters_shaque) :- write('You are in the hunter\'s shaque.'), nl.

describe_person(giulia) :- write('She is a elegantly clothed, short woman.'), nl, !.

describe_person(_) :- write('He/she is a human being.'), nl.
/* TODO add more cases */

describe_thing(giulia, watch) :- write('This is my husbands watch'), nl, !.
/* TODO add more cases */
describe_thing(_, Thing) :- write('\'A '), write(Thing), write('. What about it?\''), nl.

/* --- DEFINITIONS OF RULES --- */



/* These rules describe how to pick up an object. */

take(Thing) :-
    holding(Thing),
    write('You take the '), write(Thing), write(' out of your bag, then put it on the table, and after that you take it and put it in your pocket.'),
    !, nl.

take(Thing) :-
    i_am_at(Place),
    thing_at(Thing, Place),
    retract(thing_at(Thing, Place)),
    assert(holding(Thing)),
    write('OK.'),
    !, nl.

take(Thing) :-
    write('You try to grab the'), write(Thing), write(' or at least you try to grasp your hallucination of it.'),
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
notice :- 
    i_am_at(Place),
    describe_place(Place),
    nl,
    notice_things_at(Place),
    nl.

look :-
    i_am_at(Place),
    describe_place(Place),
    nl,
    notice_people_at(Place),
    nl.


/* These rules set up a loop to mention all the objects in your vicinity. */

notice_things_at(Place) :-
    thing_at(X, Place),
    write('There is a '), write(X), write(' here.'), nl,
    fail.

notice_things_at(_).

notice_people_at(Place) :-
    person_at(X, Place),
    write('There is '), write(X), write(' here.'), nl,
    fail.

notice_people_at(_).


/* This rules define how to talk to someon */

talk_to(thomas) :-
    write('You try talking to thomas, but the only thing you hear besides your voice is the audible confusion of giulia and andreas.'),
    !.

talk_to(Person) :-
    i_am_at(Place),
    person_at(Person, Place),
    (retract(talking_to(_)); assert(talking_to(Person))),
    write("You start talking to "), write(Person), write('.'), nl,
    (notice_things_on(Person); describe_person(Person)),
    !.

talk_to(Person) :-
    write('You start to formulate your sentence towards '), write(Person), write(', when suddenly you realise, that he cannot hear you, for he isn\'t here.'),
    nl.

notice_things_on(Person) :-
    thing_at(X, Person),
    write('There is a '), write(X), write(' on '), write(Person), write('.'), nl,
    fail.

ask_about(Thing) :-
    holding(Thing),
    talking_to(Person),
    describe_thing(Person, Thing),
    !, nl.

tell_about(Fact) :-
    i_know(Fact),
    talking_to(_),
    write('\'Okay.\''), nl.

list_facts() :-
    i_know(Fact),
    write(Fact), nl,
    fail.

/* --- REST OF DEFINITIONS --- */



/* This rule tells how to die. */

die :-
    finish.

finish :-
    nl,
    write('The game is over. Please enter the "halt." command.'),
    nl.


/* This rule just writes out game instructions. */

help :-
    nl,
    talking_to(_),
    !,
    write('ask_about(Thing)   -- to ask about a thing.'), nl,
    write('tell_about(Fact)   -- to tell about a fact.'), nl,
    write('TODO bet'), nl,
    write('TODO threaten'), nl,
    write('TODO situational yes / no'), nl,
    write('list_facts.        -- to list all known facts.'), nl,
    nl.

help :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('start.             -- to start the game.'), nl,
    write('n.  s.  e.  w.     -- to go in that direction.'), nl,
    write('take(Thing).       -- to pick up a Thing.'), nl,
    write('talk_to(Person).   -- to approach a Person.'), nl,
    write('look.              -- to look at people around you.'), nl,
    write('notice.            -- to notice things around you.'), nl,
    write('list_facts.        -- to list all known facts.'), nl,
    write('help.              -- to see this message again.'), nl,
    write('halt.              -- to end the game and quit.'), nl,
    nl.


/* This rule prints out instructions and tells where you are. */

start :-
    help,
    look.
