/* TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek. */

:- dynamic i_am_at/1, person_at/2, thing_at/2, holding/1, talking_to/1, i_know/1.
:- retractall(i_am_at(_)), retractall(person_at(_, _)), retractall(thing_at(_, _)), retractall(holding(_)), retractall(talking_to(_)), retractall(i_know(_)).


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
person_at(jonas, hotel_entrance).
person_at(urlich, hotel_entrance).

animal_at(promyczek, reception).

/* room of thomas and giulia */
thing_at(watch, room_of_thomas_and_giulia).
thing_at(thomas_journal, room_of_thomas_and_giulia).
thing_at(cigarette_light, room_of_thomas_and_giulia).

/* room of zoe */
thing_at(sleep_mask, room_of_zoe).
thing_at(sleeping_pills, room_of_zoe).
thing_at(cup, room_of_zoe).

/* bar */
thing_at(club_symbol, karl).
thing_at(glass, bar).

/* corridor */
thing_at(brooch, hilda).
thing_at(cleaning_stuff, corridor).
thing_at(cutlery_tray, corridor).

/* reception */
thing_at(guest_book, reception).
thing_at(telephone, reception).
thing_at(ball, reception).
thing_at(hunting_weapon, hermann).

/* hote entrance */
thing_at(gilded_epaulettes, urlich).
thing_at(bush, hotel_entrance).

/* hunters shque */
thing_at(bullets, hunters_shaque).
thing_at(knife_scabbard, hunters_shaque).
thing_at(blooded_knife, hunters_shaque).

/* kitchen */
thing_at(deer, kitchen).
thing_at(broth, kitchen).

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

describe_person(amy) :- write('She is dressed in the style of the 20s, with a cigarillo and sequin dress. She always wants to shine, even on a cloudy day. Her red curly hair goes well with red lipstick and red high heels. Her character is in three words: outgoing, indifferent, and heartless.'), nl, !.
describe_person(andreas) :- write('He looks like Antonio Banderas. The victim\'s brother is smartly dressed, with gold cufflinks. He has a toned body, white teeth, and dimples. His character is in three words: helpful, heartbroken, and underrated.'), nl, !.
describe_person(giulia) :- write('She is a petit, richly dressed victim\'s wife with golden hair. Her clothes are in the blood, and her strands are tousled. She looks tired. Her eyes are still tear-filled. Her character is in three words: gullible, heartbroken, and sentimental.'), nl, !.
describe_person(hans) :- write('He is a well-built elderly hotel owner with an aristocratic nose and mustache. His grey hair falls on his tired arms. On the right eye, Hans wears a monocle. His character is in three words: hard-working, righteous, and honest.'), nl, !.
describe_person(hermann) :- write('He is a muscular hunter and the owner of a dog called Promyczek. Hermann has a round face with a mustache and beard. He likes to wear khaki clothes. His character is in three words: reserved, brave, and loner.'), nl, !.
describe_person(hilda) :- write('She is a petite young cleaning lady who likes to wear modest clothes. At work, she always wears her work uniform. She has long, straight ginger hair with a golden brooch in it. Her character is in three words: orderly, chatty, and perceptive.'), nl, !.
describe_person(jonas) :- write('He is a fully-figured law student with spiky hair and smart-looking glasses. He loves jokes about Roman law. His character is in three words: hard-charging, humorous, drunkard.'), nl, !.
describe_person(jurgen) :- write('He is an exceptionally tall, slender butler. He has a long clean-shaven face. He has auburn, short and tousled hair. He likes to wear elegant clothes. His character is in three words: curt, helpful, and grave.'), nl, !.
describe_person(karl) :- write('He is a barman. He likes to wear a vest with the club symbol. His distrustful gaze allows him to keep bar\'s books in order. He has wrinkles due to frowning, but he can keep a poker face. His character is in three words: wary, crooked, wise.'), nl, !.
describe_person(stephan) :- write('He is a tall, ripped man with chubby cheeks. He likes to wear comfy clothes, especially kangaroo sweatshirts. His character is in three words: nervous, hyperactive, and romantic.'), nl, !.
describe_person(theodor) :- write('He is a joyful chubby chef with happy wrinkles. He is bold - he claims that thanks to it, he avoids problems with Sanepid. He wears a neat standard uniform with polished shoes. His character is in three words: caring, passionate, and amicable.'), nl, !.
describe_person(thomas) :- write('Thomas was an average-height, athletic man with a sun-kissed complexion. His heart-shaped face has freckles and a goatee. His shoulder-length dark hair is drenched with blood. A bloodstain and a hole made with a sharp tool on his Italian suit.'), nl, !.
describe_person(urlich) :- write('He is a doorkeeper. He wears a kind of uniform with gilded epaulets in perfect condition. He\'s not too bright, but he thinks about himself as from high society. His character is in three words: vain, gossip, reserved.'), nl, !.
describe_person(zoe) :- write('She is a gorgeous young woman with fair skin and chestnut hair. When she smiles, you can see her little dimples. Her hair is medium length, curly and golden. She wears glasses, and she loves wearing turtlenecks. Her character is in three words: shy, thoughtful, and intelligent.'), nl, !.

describe_animal(promyczek) :- write('German Spitz Miniature with keen-looking eyes. It likes caresses and it\'s well trained.'), nl, !.

prerequisites(hilda, thomas_had_been_murdered) :- \+(i_know(asked_about_brooch); i_know(theodor_trusts_me)), !.
prerequisites(urlich, watch) :- \+(i_know(poker_is_played_here)), !.
prerequisites(amy, watch) :-  \+(i_know(poker_is_played_here), i_know(watch_has_changed_hands_during_last_game)), !.
prerequisites(amy, watch_has_changed_hands_during_last_game) :-  \+(i_know(amy_won_the_watch)), !.
prerequisites(hilda, watch_has_changed_hands_during_last_game) :-  \+(i_know(zoe_befriended_hilda)), !.
prerequisites(urlich, karl) :- \+(i_know(poker_is_played_here), (person_at(urlich, Place), \+person_at(jonas, Place))), !. /* TODO should check if all people aren't here */
prerequisites(karl, andreas) :- \+(i_know(asked_andreas_about_why_is_he_here)), !.
prerequisites(andreas, andreas_was_here_yesterday) :- \+(i_know(watch_was_originally_andreases), i_know(thomas_was_here_to_buy_a_watch), i_know(asked_andreas_about_why_is_he_here), i_know(watch_has_changed_hands_during_last_game)), !.

describe_thing(hilda, brooch, asked_about_brooch) :- write('Oh, this! I\'m so glad you asked! This is a present from my dad for my 19th birthday. Beautiful, isn\'t it?'), nl, !.
describe_thing(urlich, gilded_epaulettes, poker_is_played_here) :- write('Very fine epaulets, wouldn\'t you say, dear Sir? Very fine, if I say so myself. I\'ve won these beauties the last time I won anything in our little poker game downstairs. Oh, shoot! I should not have said that!'), nl, !.
describe_thing(urlich, watch, watch_has_changed_hands_during_last_game) :- write('I saw that watch somewhere before! Isn\'t this the watch that was on our table last game? Where did you find it?'), nl, !.
describe_thing(amy, watch, amy_won_the_watch) :- write('Hey, where did you get that thing?! That\'s mine. I\'ve won it fair and square last night.'), nl, !.
describe_thing(hilda, cigarette_light, hilda_smokes_light_cigarettes) :- write('Oh, that looks like the cigarettes I smoke. Where did you get it? Thomases room? How did it get there? I wasn\'t there since yesterday\'s cleaning, and I sure as hell didn\'t leave no cigarette there!.'), nl, !.
describe_thing(amy, cigarette_light, amy_smokes_standard_cigarettes) :- write('Didn\'t know you were such a softie, detective. Wanna know what a real cigarette looks like? *shows a standard cigarette*'), nl, !.
describe_thing(karl, club_symbol, karl_likes_playing_some_card_game) :- write('\'I do like playing clubs, they\'re quite unobvious... I could rant about why it is so, but you probably don\'t even know about what game I\'m talking about, so I won\'t bother.\''), nl, !.
describe_thing(zoe, sleeping_pills, zoe_has_trouble_sleeping) :- write('I have had trouble sleeping lately, so this helps.'), nl, !.
describe_thing(giulia, thomas_journal, thomas_kept_his_journal_really_secret) :- write('\'What\'s this? That\'s belonged to Thomas? Didn\'t know he had it.\''), nl, !.
describe_thing(hilda, thomas_journal, thomas_was_here_to_buy_a_watch) :- write('\'Is that... Norwegian? Show it to me, I learned it a bit. ...well, there is something written here about a watch, and that he came here because he wanted to buy it. Whose is this journal anyway?\''), nl, !.
describe_thing(zoe, sleep_mask, zoe_has_trouble_sleeping) :- write('\'I still have jet lag. How can anyone sleep here when it\'s so early and it\'s so bright?\''), nl, !.
describe_thing(zoe, glass, zoe_parting_yesterday) :- write('\'We had a great party yesterday! I still feel that.\''), nl, !.
describe_thing(karl, glass, karl_is_barman) :- write('\'Thanks for that. I need to clean all of them before night. Don\'t get me wrong. I like my work at the bar, but people are so messy.\''), nl, !.
describe_thing(hilda, cleaning_stuff, hilda_is_cleaning_lady) :- write('\'Maybe I can do something for you? Only tell me where, and I do my best to clean that place.\''), nl, !.
describe_thing(jurgen, cutlery_tray, jurgen_is_butler) :- write('\'Anything for you, sir? At 4 o\'clock I\'ll bring dinner to your room.\''), nl, !.
describe_thing(hans, guest_book, andreas_was_here_yesterday) :- write('\'I do my best to keep the hotel papers in order. Here I save all information about visitors.\''), nl, !.
describe_thing(hans, telephone, hans_is_hotel_owner) :- write('\'The phone stopped ringing since the media heard about the murder. I hope you find the murderer soon.\''), nl, !.
describe_thing(hermann, ball, hermann_dog_is_promyczek) :- write('\'Who likes to play with the ball? My little boy.\' *huggling the dog*'), nl, !.
describe_thing(hermann, hunting_weapon, hermann_is_hunter) :- write('\'I hunted such an enormous deer yesterday. I love these forests.\''), nl, !.
describe_thing(jonas, bush, jonas_parting_yesterday) :- write('\'This is a great bush. Yesterday after the party, he helped me stay.\''), nl, !.
describe_thing(urlich, bush, ulrich_is_doorkeeper) :- write('\'See how nicely trimmed? I take care of them myself. I spend a large part of my life in this bush waiting for visitors.\''), nl, !.
describe_thing(hermann, bullets, hermann_is_hunter) :- write('\'30-caliber. Where did you get them? Aren\'t they mine?\''), nl, !.
describe_thing(hermann, knife_scabbard, hermann_is_hunter) :- write('\'My family\'s coat of arms is on the scabbard. This knife was given to me by my father. He was a hunter too.\''), nl, !.
describe_thing(hermann, blooded_knife, hermann_lost_knife) :- write('\'Oh, someone finally found it! After taking the deer to the kitchen, the knife was lost. I thought it had fallen on the way back.\''), nl, !.
describe_thing(theodor, deer, theodor_is_chef) :- write('\'Hermann brought it yesterday. Poor animal, but it won\'t be wasted. There will be a delicious stew tomorrow. I hope you will stay.\''), nl, !.
describe_thing(theodor, broth, theodor_trusts_me) :- write('\'Do you really like my soup? I know that you are a great men.\''), nl, !.
describe_thing(_, Thing, _) :- write('\'A '), write(Thing), write('. What about it?\''), nl.

describe_fact(hilda, thomas_had_been_murdered, poker_is_played_here) :- write('\'I don\'t really know anything about this, but... I do know that he has been playing poker with some other people here. Maybe something went wrong there?\''), nl, !.
describe_fact(amy, watch_has_changed_hands_during_last_game, amy_passed_out) :- write('Yeah, I won the game last night, and the watch with it. I think I passed out and lost it when I was returning to my room last night. I mean, I drank a bit, but no more than usual, and I never pass out. The weirdest thing. But I swear, I woke up the next day and the thing was gone!'), nl, !.
describe_fact(hilda, watch_has_changed_hands_during_last_game, zoe_knew_about_watch_changing_hands) :- write('Oh yeah, Amy won the watch yesterday. That watch surely must\'ve cost a lot. I was so shocked when it appear on the table. When I told this to Zoe, she also couldn\'t believe this.'), nl, !.
describe_fact(giulia, thomas_had_been_murdered, giulia_is_heart_broken) :- write('\'What I need to do? Is he trully dead? He cannot be. He promised. I want him back...\''), nl, !.
describe_fact(karl, poker_is_played_here, ulrich_has_an_open_mouth) :- write('\'Who told you - it was Urlich, wasn\'t it? He never could keep his mouth shut. Yes, we do like to play some poker around here, at different stakes. Since you already know about it, maybe you\'d like to give it a try?\''), nl, !.
describe_fact(karl, karl_cheats_at_poker, karl_trusts_me) :- write('\'So, you\'re a pretty good detective, aren\'t you? Well, you got me. I\'ll tell you what you want.\''), nl, !.
describe_fact(andreas, thomas_was_here_to_buy_a_watch, watch_was_originally_andreases) :- write('\'Looks like nothing is a secret to you, huh? Yes, this watch was mine and yes, I wanted to sell it, but I found out that Thomas was the buyer and I just couldn\'t show him that I\'m poor just like that. And I certainly did not kill him!\''), nl, !.
describe_fact(andreas, andreas_was_here_yesterday, andreas_needs_money) :- write('\'Yes, I was here. I\'m sorry that I lied to you earlier. I really don\'t like anyone noticing that my life is not as great as I want people to see it. I was here, because I wanted to sell my watch to get some money, because I really need them right now. I found out that Thomas was the buyer just yesterday, and I couldn\'t bear the fact that he would know. So I tried my luck in cards, and obviously, I lost it.\'')
describe_fact(karl, karl_cheats_at_poker, amy_knows_about_karl_cheating_at_poker) :- write('\'That scoundrel! Thanks for letting me know, mate. I\'ll keep an eye on him next time.\''), nl, !.
/* TODO add more cases */
describe_fact(_, _, _) :- write('\'Okay.\''), nl, !.

describe_gossip(hilda, zoe, zoe_befriended_hilda) :- write('Well, it was at hard at the beginning, but once you get to know her, she\'s a really sweet and nice person. We talked quite a lot lately.'), nl, !.
describe_gossip(amy, zoe, zoe_was_thomas_lovers) :- write('Oh, yeah, her. Not much I can say about her except that she most probably slept with Thomas. They were all drooly towards each other.'), nl, !.
describe_gossip(amy, thomas, zoe_was_thomas_lovers) :- write('Well, I was once his girlfriend. Probably won\'t be again, will I? Hahah. Oh, don\'t look at me like that, he always wanted to have everything, that\'s how people like that end. But damn, he looked good. If you ask me, I bet it has something to do with that Zoe and Giulia. Him and Zoe looked like they have done something that Giulia might not have liked...'), nl, !.
describe_gossip(urlich, karl, karl_cheats_at_poker) :- write('Is no one around? Fine. If Sir really wants to know, I believe karl is cheating during our little poker games! I saw him once pull out an ace from his sleeve.'), nl, !.
describe_gossip(karl, andreas, andreas_was_here_yesterday) :- write('About that one I can say something, I heard he told you that he came here today, while in fact, he was playing with us yesterday.'), nl, !.
/* TODO add more cases */
describe_gossip(_, _, _) :- write('\'Not much I can say about him/her.\''), nl, !.

describe_stay_reason(thomas) :- write('\'Dead man tell no tales\''), !.
describe_stay_reason(giulia) :- write('\'I came here with Thomas on vacations.\'').
describe_stay_reason(andreas) :- (\+i_know(asked_andreas_about_why_is_he_here); assert(i_know(asked_andreas_about_why_is_he_here))), write('\'I came here as soon I heard that my brother is... he is dead!\''), !.
describe_stay_reason(zoe) :- write('\'It\'s always nice to spend some free time in such a beatiful place, isn\'t it?\''), !.
describe_stay_reason(amy) :- write('\'I heard there are some oppuritinities to get some cash here.\''), !.
describe_stay_reason(stephan) :- write('\'I wanted to talk to Thomas and heard he will be here. I won\'t be able to now...\''), !.
describe_stay_reason(hermann) :- write('\'Well, I live nearby and hunt for Thomas some deers sometime, like yesterday.\''), !.
describe_stay_reason(jonas) :- write('\'I live nearby and I heard about the murder, so I might gain some real life experience in this case.\''), !.
describe_stay_reason(_) :- write('\'Well, I work here\''), !.


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


/* These rules describe how to pet the dog */

pet :-
    talking_to(promyczek),
    write('Pet the dog.'),
    nl.

play :- 
    talking_to(promyczek),
    holding(ball),
    write('Play ball with the dog.'),
    nl,
    !.

play :-
    talking_to(promyczek),
    write('You want to play with the dog, but you don\'t have his favorite ball.'), nl.

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
    nl,
    notice_animals_at(Place),
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

notice_animals_at(Place) :-
    animal_at(X, Place),
    write('There is '), write(X), write(' here.'), nl,
    fail.

notice_animals_at(_).

/* This rules define how to talk to someone */

talk_to(Animal) :-
    i_am_at(Place),
    animal_at(Animal, Place),
    (retractall(talking_to(_)); \+fail),
    assert(talking_to(Animal)),
    write('You start playing with'), write(Animal), write('.'), nl,
    describe_animal(Animal),
    !.

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
    \+ prerequisites(Person, Thing),
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
    \+ prerequisites(Person, Fact),
    describe_fact(Person, Fact, DiscoveredFact),
    \+ i_know(DiscoveredFact),
    assert(i_know(DiscoveredFact)),
    write('NEW FACT ADDED'),
    nl.

tell_about(Fact) :-
    i_know(Fact),
    talking_to(Person),
    tell_about(Fact, Person).

gossip_about(SomePerson, Person) :-
    \+ prerequisites(Person, SomePerson),
    describe_gossip(Person, SomePerson, DiscoveredFact),
    \+ i_know(DiscoveredFact),
    assert(i_know(DiscoveredFact)),
    write('NEW FACT ADDED'),
    nl.

gossip_about(SomePerson) :-
    talking_to(Person),
    gossip_about(SomePerson, Person).

why_here() :-
    talking_to(Person),
    describe_stay_reason(Person).

list_facts() :-
    i_know(Fact),
    write(Fact), nl,
    fail.

list_facts().

accuse(zoe) :-
    talking_to(hans),
    i_know(zoe_knew_about_watch_changing_hands),
    holding(sleeping_pills),
    i_know(zoe_knew_about_giulia),
    i_know(amy_passed_out),
    !,
    write('Congratulations, you\'ve won!'),
    halt.

accuse(_) :-
    talking_to(hans),
    write('\'You\'ve got to have more proof for such a bold statement, young man.\'').



/* --- REST OF DEFINITIONS --- */

introduction :-
    write('Daily life in the alpine hotel was disrupted by the barking of the Promyczek. The wife, which came after that to the room, saw her husband on the floor. She started screaming and woke up everyone who stayed in the hotel. The hotel owner immediately calls you to solve that riddle. Who is the murderer? You don\'t know. Yet...'),
    nl, nl.

/* This rule just writes out game instructions. */

help :-
    nl,
    talking_to(promyczek),
    write('play               -- play ball with the Promyczek.'), nl,
    write('pet                -- pet the dog.'),
    write('take(Thing).       -- to pick up a Thing.'), nl,
    nl,
    !.

help :-
    nl,
    talking_to(_),
    \+((talking_to(hans), write('accuse(Person)     -- to accuse a person of murder.'), nl, fail)),
    !,
    write('ask_about(Thing)   -- to ask about a thing.'), nl,
    write('tell_about(Fact)   -- to tell about a fact.'), nl,
    write('gossip_about(Person) -- to gossip about a person.'), nl,
/*  TODO write('bet'), nl,
    TODO write('threaten'), nl, */
    write('why_here.          -- to ask your interlocutor why is he here'), nl, */
    write('list_facts.        -- to list all known facts.'), nl,
    nl.

help :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('n.  s.  e.  w.     -- to go in that direction.'), nl,
    write('take(Thing).       -- to pick up a Thing.'), nl,
    write('talk_to(Person).   -- to approach a Person.'), nl,
    write('look.              -- to look at people around you.'), nl,
    write('notice.            -- to notice things around you.'), nl,
    write('list_facts.        -- to list all known facts.'), nl,
    write('help.              -- to see this message again.'), nl,
    write('halt.              -- to end the game and quit.'), nl,
    nl.

:- help, introduction, look.
