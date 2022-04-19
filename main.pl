/* TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek. */

:- dynamic i_am_at/1, person_at/2, thing_at/2, holding/1, talking_to/1, i_know/1.
:- retractall(person_at(_, _)), retractall(thing_at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(i_know(_)).

/** facts:
*     done - thomas_had_been_murdered
*     done - poker_is_played_here
*     done - murderer_had_a_watch
*     done - watch_has_changed_hands_during_last_game
*     done - zoe_befriended_hilda
*     done - zoe_was_thomas_lovers
*     done - zoe_knew_about_watch_changing_hands
*     zoe_has_chloroform
*     done - amy_passed_out
*     done - amy_won_the_watch
*     zoe_knew_about_giulia
*
*     done - asked_about_broche
*     theodor_trusts_me
*     TODO more
*/



/* --- DEFINITIONS OF PEOPLE, THINGS, PLACES AND DIALOGUES --- */



/*  These rules defines connections between places */

path(room_of_thomas_and_giulia, w, corridor).

path(corridor, e, room_of_thomas_and_giulia).
path(corridor, w, room_of_zoe).
path(corridor, s, reception).

path(room_of_zoe, e, corridor).

path(reception, n, corridor).
path(reception, w, bar).
path(reception, s, hotel_entrance).

path(bar, e, reception).
path(bar, s, kitchen).

path(hotel_entrance, n, reception).
path(hotel_entrance, s, hunters_shaque).

path(kitchen, n, bar).
path(kitchen, s, hunters_shaque).

path(hunters_shaque, w, hotel_entrance).
path(hunters_shaque, e, kitchen).

/* These rules describe where everything and everyone is. */

i_am_at(room_of_thomas_and_giulia).
person_at(thomas, room_of_thomas_and_giulia).
person_at(giulia, room_of_thomas_and_giulia).
person_at(andreas, room_of_thomas_and_giulia).
person_at(zoe, room_of_zoe).
person_at(karl, bar).
person_at(amy, bar).
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
thing_at(club_symbol, karl).
thing_at(gilded_epaulettes, urlich).

holding(money).

/* The facts that the player knows about from start */

i_know(thomas_had_been_murdered).

/* These rules set up various user messages in the game */

describe_place(room_of_thomas_and_giulia) :- write('You are in room of Thomas and Giulia.'), nl.
describe_place(room_of_zoe) :- write('You are in room of Zoe.'), nl.
describe_place(bar) :- write('You are in the bar.'), nl.
describe_place(corridor) :- write('You are in the corridor.'), nl.
describe_place(kitchen) :- write('You are in the kitchen.'), nl.
describe_place(reception) :- write('You are in the reception.'), nl.
describe_place(hotel_entrance) :- write('You are at the hotels entrance.'), nl.
describe_place(hunters_shaque) :- write('You are in the hunter\'s shaque.'), nl.

describe_person(amy) :- write('She is dressed like in the 20s, with cigarillo and sequin dress. She always want to shine, even in cloudy day. Her red curly hair go well with red lipstick and red high heels. Her character in three words: outgoing, indiferent, heartless.'), nl, !.
describe_person(andreas) :- write('He looks like Antiono Banderas. He is smartly dressed, with gold cufflinks. He has toned body, white teeth and dimples. His character in three words: helpful, heart-broken, underrated.'), nl, !.
describe_person(giulia) :- write('She is a petit, richly dressed victim\'s wife, with golden hair. Her clothes are in blood, her hairs are tousled. She looks tired, her eyes are still tear-filled. Her character in three words: gullible, heart-broken,  sentimental.'), nl, !.
describe_person(hans) :- write('He is a well-build elderly hotel owner, with an aristrocratic nose and moustache. His grey hair fall on the tired arms. He wears a monocle on the right eye. His character in three words: hard-working, righteous, honest.'), nl, !.
describe_person(hermann) :- write('He is a muscular hunter and the owner of the dog. He has a round face with moustache and beard. He likes to wear khaki clothes.  His character in three words: reserved, brave, loner.'), nl, !.
describe_person(hilda) :- write('She is a petite young cleaning lady, who likes to wear modest clothes. In work she always wear work uniform. She has long straight ginger hair with a golden broche in it. Her character in three words: orderly, chatty, perceptive.'), nl, !.
describe_person(jonas) :- write('He is a fully-figured law student with spiky hair and smart looking glasses. He loves jokes about roman law. His character in three words: hard-charging, humorous, drunkard.'), nl, !.
describe_person(jurgen) :- write('He is extremely tall, slender butler. He has long clean-shaven face. He has auburn, short and tousled hair. He likes to wear elegant clothes. His character in three words: curt, helpful, grave'), nl, !.
describe_person(karl) :- write('He is a barman. He likes to wear a vest with the club symbol. His distrustful gaze allows him to keep bar\'s books in order. He has wrinkles due to frowing, but he can keep a poker face. His character in three words: wary, crooked, smart.'), nl, !.
describe_person(promyczek) :- write('German Spitz Miniature with keen looking eyes. It likes caressnes and it\'s well trained.'), nl, !.
describe_person(stephan) :- write('He is a tall, ripped man with chubby cheeks. He likes to wear comfy clothes, especially kangaroo sweatshirts. His character in three words: nervous, hyperactive, romantic.'), nl, !.
describe_person(theodor) :- write('He is a joyful chubby chef with happiness wrinkles. He is bold - he claims that thanks to it, he avoid problems with Sanepid. He wears standard neat uniform with polished shoes. His character in three words: caring, passionate, amicable.'), nl, !.
describe_person(thomas) :- write('He was a average-height, athletic man, with a sun-kissed complexion. His heart-shaped face has freckles and a goatee. His shoulder-length dark hairs is drenched with blood. On his Italian suit, there is a blood stain and a hole made with a sharp tool.'), nl, !.
describe_person(urlich) :- write('He is a doorkeeper. He wears a kind of uniform with gilded epaulettes in perfect condition. He\'s not to bright, but he thinks about himself as from high society. His character in three words: vain, gossip, reserved.'), nl, !.
describe_person(zoe) :- write('She is a gorgeous young women with fair skin and chestnut hair. When she smiles, you can see her little dimples. Her hair are medium lenght, curly and fair. She wear glasses and she loves wearing turtlenecks. Her character in three words: shy, thoughtful, smart.'), nl, !.

prerequisites(thomas_had_been_murdered, hilda) :- \+(i_know(asked_about_broche); i_know(theodor_trusts_me)), !.
prerequisites(watch, urlich) :- \+(i_know(poker_is_played_here)), !.
prerequisites(watch, amy) :-  \+(i_know(poker_is_played_here), i_know(watch_has_changed_hands_during_last_game)), !.
prerequisites(watch_has_changed_hands_during_last_game, amy) :-  \+(i_know(amy_won_the_watch)), !.
prerequisites(watch_has_changed_hands_during_last_game, hilda) :-  \+(i_know(zoe_befriended_hilda)), !.

describe_thing(hilda, broche, asked_about_broche) :- write('Oh, this! I\'m so glad you asked! This is a present from my dad for my 19th birthday. Beautiful, isn\'t it?'), nl, !.
describe_thing(urlich, gilded_epaulettes, poker_is_played_here) :- write('Very fine epaulettes, wouldn\'t you say dear Sir? Very fine, if I say so myself. I\'ve won these beauties the last time I won anything in our little poker game downstairs. Oh, shoot! I should not have said that!'), nl, !.
describe_thing(urlich, watch, watch_has_changed_hands_during_last_game) :- write('I saw that watch somewhere before! Isn\'t this the watch that was on our table last game? Where did you find it?'), nl, !.
describe_thing(amy, watch, amy_won_the_watch) :- write('Hey, where did you get that thing?! That\'s mine. I\'ve won it fair and square last night'), nl, !.
/* TODO add more cases */
describe_thing(_, Thing, _) :- write('\'A '), write(Thing), write('. What about it?\''), nl.

describe_fact(thomas_had_been_murdered, hilda, poker_is_played_here) :- write('\'I don\'t really know anything about this, but... I do know that he has been playing poker with some other people here. Maybe something went wrong there?\''), nl, !.
describe_fact(watch_has_changed_hands_during_last_game, amy, amy_passed_out) :- write('Yeah, I won the game last night, and the watch with it. I think I passed out and lost it when I was returning to my room last night. I mean, I drank a bit, but no more than usual, and I never pass out. The weirdest thing. But I swear, I woke up the next day and the thing was gone!'), nl, !.
describe_fact(watch_has_changed_hands_during_last_game, hilda, zoe_knew_about_watch_changing_hands) :- write('Oh yeah, Amy won the watch yesterday. That watch surely must\'ve cost a lot. I was so shocked when it appear on the table. When I told this to Zoe, she also couldn\'t believe this.'), nl, !.
/* TODO add more cases */
describe_fact(_, _, _) :- write('\'Okay.\''), nl, !.

describe_gossip(zoe, hilda, zoe_befriended_hilda) :- write('Well, it was at hard at the beginning, but once you get to know her, she\'s a really sweet and nice person. We talked quite a lot lately.'), nl, !.
describe_gossip(zoe, amy, zoe_was_thomas_lovers) :- write('Oh, yeah, her. Not much I can say about her except that she most probably slept with Thomas. They were all drooly towards each other.'), nl, !.
describe_gossip(thomas, amy, zoe_was_thomas_lovers) :- write('Well, I was once his girlfriend. Probably won\'t be again, will I? Hahah. Oh, don\'t look at me like that, he always wanted to have everything, that\'s how people like that end. But damn, he looked good. If you ask me, I bet it has something to do with that Zoe and Giulia. Him and Zoe looked like they have done something that Giulia might not have liked...'), nl, !.
/* TODO add more cases */
describe_gossip(_, _, _) :- write('\'Not much I can say about him/her.\''), nl, !.



/* --- DEFINITIONS OF ACTIONS --- */



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
    (retractall(talking_to(_)); \+fail),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look.

go(_) :-
    write('You can''t go that way.').

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

notice_things_on(Person) :-
    thing_at(X, Person),
    write('There is a '), write(X), write(' on '), write(Person), write('.'), nl,
    fail.

notice_things_on(_).


/* This rules define how to talk to someone */

talk_to(thomas) :-
    i_am_at(Place),
    person_at(thomas, Place),
    write('You try talking to thomas, but the only thing you hear besides your voice is the audible confusion of giulia and andreas.'),
    !.

talk_to(Person) :-
    i_am_at(Place),
    person_at(Person, Place),
    (retractall(talking_to(_)); \+fail),
    assert(talking_to(Person)),
    write('You start talking to '), write(Person), write('.'), nl,
    notice_things_on(Person),
    describe_person(Person),
    !.

talk_to(Person) :-
    write('You start to formulate your sentence towards '), write(Person), write(', when suddenly you realise, that he cannot hear you, for he isn\'t here.'),
    nl.

ask_about(Thing, Person) :-
    \+ prerequisites(Thing, Person),
    describe_thing(Person, Thing, DiscoveredFact),
    \+ i_know(DiscoveredFact),
    assert(i_know(DiscoveredFact)),
    write('NEW FACT ADDED'),
    nl.

ask_about(Thing) :-
    (holding(Thing); thing_at(Thing, Person)),
    talking_to(Person),
    ask_about(Thing, Person).

tell_about(Fact, Person) :-
    \+ prerequisites(Fact, Person),
    describe_fact(Fact, Person, DiscoveredFact),
    \+ i_know(DiscoveredFact),
    assert(i_know(DiscoveredFact)),
    write('NEW FACT ADDED'),
    nl.

tell_about(Fact) :-
    i_know(Fact),
    talking_to(Person),
    tell_about(Fact, Person).

gossip_about(SomePerson, Person) :-
    \+ prerequisites(SomePerson, Person),
    describe_gossip(SomePerson, Person, DiscoveredFact),
    \+ i_know(DiscoveredFact),
    assert(i_know(DiscoveredFact)),
    write('NEW FACT ADDED'),
    nl.

gossip_about(SomePerson) :-
    talking_to(Person),
    gossip_about(SomePerson, Person).

list_facts() :-
    i_know(Fact),
    write(Fact), nl,
    fail.

list_facts().

accuse(zoe) :-
    talking_to(hans),
    i_know(zoe_knew_about_watch_changing_hands),
    holding(bottle_of_chloroform),
    i_know(zoe_knew_about_giulia),
    i_know(amy_passed_out),
    !,
    write('Congratulations, you\'ve won!'),
    halt.

accuse(_) :-
    talking_to(hans),
    write('\'You\'ve got to have more proof for such a bold statement, young man.\'').



/* --- REST OF DEFINITIONS --- */



/* This rule just writes out game instructions. */

help :-
    nl,
    talking_to(_),
    \+((talking_to(hans), write('accuse(Person)     -- to accuse a person of murder.'), nl, fail)),
    !,
    write('ask_about(Thing)   -- to ask about a thing.'), nl,
    write('tell_about(Fact)   -- to tell about a fact.'), nl,
    write('gossip_about(Person) -- to gossip about a person'), nl,
/*  TODO write('bet'), nl,
    TODO write('threaten'), nl,
    TODO write('situational yes / no'), nl, */
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
