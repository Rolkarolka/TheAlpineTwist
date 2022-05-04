module Person where
    import Prelude
    import qualified Control.Monad as Monad
    import qualified Data.Maybe as Maybe
    import qualified Data.List as List

    import Item
    import State
    import Utilities

    people_descriptions =
        [ ("amy", "She is dressed in the style of the 20s, with a cigarillo and sequin dress. She always wants to shine, even on a cloudy day. Her red curly hair goes well with red lipstick and red high heels. Her character is in three words: outgoing, indifferent, and heartless.")
        , ("andreas", "He looks like Antonio Banderas. The victim's brother is smartly dressed, with gold cufflinks. He has a toned body, white teeth, and dimples. His character is in three words: helpful, heartbroken, and underrated.")
        , ("giulia", "She is a petit, richly dressed victim's wife with golden hair. Her clothes are in the blood, and her strands are tousled. She looks tired. Her eyes are still tear-filled. Her character is in three words: gullible, heartbroken, and sentimental.")
        , ("hans", "He is a well-built elderly hotel owner with an aristocratic nose and mustache. His grey hair falls on his tired arms. On the right eye, Hans wears a monocle. His character is in three words: hard-working, righteous, and honest.")
        , ("hermann", "He is a muscular hunter and the owner of a dog called Promyczek. Hermann has a round face with a mustache and beard. He likes to wear khaki clothes. His character is in three words: reserved, brave, and loner.")
        , ("hilda", "She is a petite young cleaning lady who likes to wear modest clothes. At work, she always wears her work uniform. She has long, straight ginger hair with a golden brooch in it. Her character is in three words: orderly, chatty, and perceptive.")
        , ("jonas", "He is a fully-figured law student with spiky hair and smart-looking glasses. He loves jokes about Roman law. His character is in three words: hard-charging, humorous, drunkard.")
        , ("jurgen", "He is an exceptionally tall, slender butler. He has a long clean-shaven face. He has auburn, short and tousled hair. He likes to wear elegant clothes. His character is in three words: curt, helpful, and grave.")
        , ("karl", "He is a barman. He likes to wear a vest with the clubs symbol. His distrustful gaze allows him to keep bar's books in order. He has wrinkles due to frowning, but he can keep a poker face. His character is in three words: wary, crooked, smart.")
        , ("stephan", "He is a tall, ripped man with chubby cheeks. He likes to wear comfy clothes, especially kangaroo sweatshirts. His character is in three words: nervous, hyperactive, and romantic.")
        , ("theodor", "He is a joyful chubby chef with happy wrinkles. He is bold - he claims that thanks to it, he avoids problems with Sanepid. He wears a neat standard uniform with polished shoes. His character is in three words: caring, passionate, and amicable.")
        , ("thomas", "Thomas was an average-height, athletic man with a sun-kissed complexion. His heart-shaped face has freckles and a goatee. His shoulder-length dark hair is drenched with blood. A bloodstain and a hole made with a sharp tool on his Italian suit.")
        , ("urlich", "He is a doorkeeper. He wears a kind of uniform with gilded epaulets in perfect condition. He's not too bright, but he thinks about himself as from high society. His character is in three words: vain, gossip, reserved.")
        , ("zoe", "She is a gorgeous young woman with fair skin and chestnut hair. When she smiles, you can see her little dimples. Her hair is medium length, curly and golden. She wears glasses, and she loves wearing turtlenecks. Her character is in three words: shy, thoughtful, and intelligent.")
        ]

    prerequisites = 
        [ (("hilda", "thomas_had_been_murdered"), (\(state) -> elem "asked_about_brooch" (known_facts state) || elem "theodor_trusts_me" (known_facts state)))
        , (("urlich", "watch"), (\(state) -> elem "poker_is_played_here" (known_facts state)))
        , (("amy", "watch"), (\(state) -> elem "poker_is_played_here" (known_facts state) && elem "watch_has_changed_hands_during_last_game" (known_facts state)))
        , (("amy", "watch_has_changed_hands_during_last_game"), (\(state) -> elem "amy_won_the_watch" (known_facts state)))
        , (("hilda", "watch_has_changed_hands_during_last_game"), (\(state) -> elem "zoe_befriended_hilda" (known_facts state)))
        , (("urlich", "karl"), (\(state) -> elem "poker_is_played_here" (known_facts state))) -- \+ ((i_know(poker_is_played_here), person_at(urlich, Place), \+ ((person_at(Person, Place), Person \= urlich)))), !.
        , (("karl", "andreas"), (\(state) -> elem "asked_andreas_why_is_he_here" (known_facts state)))
        , (("andreas", "andreas_was_here_yesterday"), (\(state) -> elem "watch_was_originally_andreases" (known_facts state) && elem "thomas_was_here_to_buy_a_watch" (known_facts state) && elem "asked_andreas_why_is_he_here" (known_facts state) && elem "watch_has_changed_hands_during_last_game" (known_facts state)))
        -- , (("jonas", "karl"), (\(state) -> (people_at state))) -- \+ ((person_at(jonas, bar))), !.
        , (("hilda", "zoe_was_thomas_lovers"), (\(state) -> elem "zoe_befriended_hilda" (known_facts state)))
        ]

    describe =
        [ ["hilda",     "brooch",            "asked_about_brooch",                    "'Oh, this! I'm so glad you asked! This is a present from my dad for my 19th birthday. Beautiful, isn't it?'"]
        , ["urlich",    "gilded_epaulettes", "poker_is_played_here",                  "'Very fine epaulets, wouldn't you say, dear Sir? Very fine, if I say so myself. I've won these beauties the last time I won anything in our little poker game downstairs. Oh, shoot! I should not have said that!'"]
        , ["urlich",    "watch",             "watch_has_changed_hands_during_last_game", "'I saw that watch somewhere before! Isn't this the watch that was on our table last game? Where did you find it?'"]
        , ["amy",       "watch",             "amy_won_the_watch",                     "'Hey, where did you get that thing?! That's mine. I've won it fair and square last night.'"]
        , ["hilda",     "cigarette_light",   "hilda_smokes_light_cigarettes",         "'Oh, that looks like the cigarettes I smoke. Where did you get it? Thomases room? How did it get there? I wasn't there since yesterday's cleaning, and I sure as hell didn't leave no cigarette there!.'"]
        , ["amy",       "cigarette_light",   "amy_smokes_standard_cigarettes",        "'Didn't know you were such a softie, detective. Wanna know what a real cigarette looks like? *shows a standard cigarette*'"]
        , ["karl",      "clubs_symbol",      "karl_likes_playing_some_card_game",     "'I do like playing clubs, they're quite unobvious... I could rant about why it is so, but you probably don't even know about what game I'm talking about, so I won't bother.'"]
        , ["zoe",       "sleeping_pills",    "zoe_has_trouble_sleeping",              "'I have had trouble sleeping lately, so this helps.'"]
        , ["giulia",    "thomas_journal",    "thomas_kept_his_journal_really_secret", "'What's this? That's belonged to Thomas? Didn't know he had it.'"]
        , ["hilda",     "thomas_journal",    "thomas_was_here_to_buy_a_watch",        "'Is that... Norwegian? Show it to me, I learned it a bit. ...well, there is something written here about a watch, and that he came here because he wanted to buy it. Whose is this journal anyway?'"]
        , ["zoe",       "sleep_mask",        "zoe_has_trouble_sleeping",              "'I still have jet lag. How can anyone sleep here when it's so early and it's so bright?'"]
        , ["zoe",       "glass",             "zoe_parting_yesterday",                 "'We had a great party yesterday! I still feel that.'"]
        , ["karl",      "glass",             "karl_is_barman",                        "'Thanks for that. I need to clean all of them before night. Don't get me wrong. I like my work at the bar, but people are so messy.'"]
        , ["hilda",     "cleaning_stuff",    "hilda_is_cleaning_lady",                "'Maybe I can do something for you? Only tell me where, and I do my best to clean that place.'"]
        , ["jurgen",    "cutlery_tray",      "jurgen_is_butler",                      "'Anything for you, sir? At 4 o'clock I'll bring dinner to your room.'"]
        , ["hans",      "guest_book",        "andreas_was_here_yesterday",            "'I do my best to keep the hotel papers in order. Here I save all information about visitors.'"]
        , ["hans",      "telephone",         "hans_is_hotel_owner",                   "'The phone stopped ringing since the media heard about the murder. I hope you find the murderer soon.'"]
        , ["hermann",   "ball",              "hermann_dog_is_promyczek",              "'Who likes to play with the ball? My little boy.' *huggling the dog*"]
        , ["hermann",   "hunting_weapon",    "hermann_is_hunter",                     "'I hunted such an enormous deer yesterday. I love these forests.'"]
        , ["jonas",     "bush",              "jonas_parting_yesterday",               "'This is a great bush. Yesterday after the party, he helped me stay straight. The most comfortable bush I've ever slept under.'"]
        , ["urlich",    "bush",              "ulrich_is_doorkeeper",                  "'See how nicely trimmed? I take care of them myself. I spend a large part of my life in this bush waiting for visitors.'"]
        , ["hermann",   "bullets",           "hermann_is_hunter",                     "'30-caliber. Where did you get them? Aren't they mine?'"]
        , ["hermann",   "knife_scabbard",    "hermann_is_hunter",                     "'My family's coat of arms is on the scabbard. This knife was given to me by my father. He was a hunter too.'"]
        , ["hermann",   "blooded_knife",     "hermann_lost_knife",                    "'Oh, someone finally found it! After taking the deer to the kitchen, the knife was lost. I thought it had fallen on the way back.'"]
        , ["theodor",   "deer",              "theodor_is_chef",                       "'Hermann brought it yesterday. Poor animal, but it won't be wasted. There will be a delicious stew tomorrow. I hope you will stay.'"]
        , ["theodor",   "broth",             "theodor_trusts_me",                     "'Do you really like my soup? I know that you are a great man.'"]
        , ["giulia",    "thomas_had_been_murdered",                 "giulia_is_heart_broken",                   "'What I'm suppose to do? Is he trully dead? He cannot be. He promised. I want him back...'"]
        , ["amy",       "watch_has_changed_hands_during_last_game", "amy_passed_out",                           "'Yeah, I won the game last night, and the watch too. I think I passed out and lost it when I was returning to my room last night. I mean, I drank a bit, but not more than usual, and normally I don't even feel drunk, let alone pass out. The weirdest feeling. But I swear, when I woke up the next day the thing was gone!'"]
        , ["amy",       "karl_cheats_at_poker",                     "amy_knows_about_karl_cheating_at_poker",   "'That scoundrel! Thanks for letting me know, mate. I'll keep an eye on him next time.'"]
        , ["hilda",     "thomas_had_been_murdered",                 "poker_is_played_here",                     "'I don't really know anything about this, but... I do know that he has been playing poker with some other people here. Maybe something went wrong there?'"]
        , ["hilda",     "watch_has_changed_hands_during_last_game", "zoe_knew_about_watch_changing_hands",      "'Oh yeah, Amy won the watch yesterday. That watch surely must've cost a lot. I was so shocked when it appear on the table. When I told this to Zoe, she also couldn't believe this.'"]
        , ["giulia",    "thomas_had_been_murdered",                 "giulia_is_heart_broken",                   "'What I'm suppose to do? Is he trully dead? He cannot be. He promised. I want him back...'"]
        , ["karl",      "poker_is_played_here",                     "ulrich_has_an_open_mouth",                 "'Who told you - it was Urlich, wasn't it? He never could keep his mouth shut. Yes, we do like to play some poker around here, at different stakes. Since you already know about it, maybe you'd like to give it a try?'"]
        , ["karl",      "karl_cheats_at_poker",                     "karl_trusts_me",                           "'So, you're a pretty good detective, aren't you? Well, you got me. I'll tell you what you want.'"]
        , ["andreas",   "thomas_was_here_to_buy_a_watch",           "watch_was_originally_andreases",           "'Looks like nothing is a secret to you, huh? Yes, this watch was mine and yes, I wanted to sell it, but I found out that Thomas was the buyer and I just couldn't let him know that I'm penniless just like that. And I certainly did not kill him!'"]
        , ["andreas",   "andreas_was_here_yesterday",               "andreas_needs_money",                      "'Yes, I was here. I'm sorry that I lied to you earlier. I really don't like anyone noticing that my life is not as great as I want people to see it. I was here, because I wanted to sell my watch to get some money. I really need them right now. I found out that Thomas was the buyer just yesterday, and I couldn't bear the fact that he would know. So I tried my luck in cards, and obviously, I lost it.'"]
        , ["jonas",     "jonas_likes_drinking_in_company",          "jonas_went_to_bar",                        "'Of course I love drinking in good company, who doesn't? By the way, it's about the best time to go drinking together! I'll go save a table for us in the bar and you go get the drinks!'"]
        , ["hilda",     "zoe_was_thomas_lovers",                    "zoe_did_not_know_about_giulia",            "'WHAT?! Her? No way! You're serious?! She told me she didn't even knew who they were! That's... interesting to say the least.'"]
        , ["amy",       "thomas",   "zoe_was_thomas_lovers",            "'Well, I once was his girlfriend. Probably won't be again, will I? Hahah. Oh, don't look at me like that, he always wanted to have everything, that's how people like him end. But damn, he looked good. If you ask me, I bet it has something to do with that Zoe girl and Giulia. Him and Zoe looked like they have done something that Giulia might not have liked...'"]
        , ["amy",       "zoe",      "zoe_was_thomas_lovers",            "'Oh, yeah, her. Not much I can say about her except that she most probably slept with Thomas. They were all drooly towards each other.'"]
        , ["amy",       "karl",     "karl_likes_betting",               "'Karl is a nice guy, but he won't tell you any important information, unless... Unless you bet him. He's a gambler, he can't refuse a good bet.'"]
        , ["jurgen",    "karl",     "karl_likes_betting",               "'Karl doesn't trust almost anyone, but I'll give you an advice - if you want to get some important information from him, try betting him instead of asking straight forward.'"]
        , ["giulia",    "thomas",   "thomas_had_been_murdered",         "As soon as you mention Thomas' name, Giulia starts crying again..."]
        , ["hans",      "karl",     "karl_has_a_keen_eye",              "'Karl has a keen eye, nothing slips past him. He probably can help you.'"]
        , ["hilda",     "karl",     "karl_has_a_keen_eye",              "'Karl has a keen eye, nothing slips past him. He probably can help you.'"]
        , ["hilda",     "thomas",   "thomas_had_been_murdered",         "'Poor Thomas... I knew him a bit - he used to work here with his good friend back when he was younger. I wonder if their friendship survived through all this time...'"]
        , ["hilda",     "zoe",      "zoe_befriended_hilda",             "'Well, it was at hard at the beginning, but once you get to know her, she's a really sweet and nice person. We talked quite a lot lately.'"]
        , ["jonas",     "karl",     "karl_admitted_to_cheating",        "'This guy is a crook, I'll tell you that! *hic!* We were drinking together once and he told me *hic!* that he's cheating all the time! He was so proud that no one can catch him red-handed. *hic!* But I promised that I wouldn't tell anyone, so you have to promise me, too! *hic!*'"]
        , ["karl",      "andreas",  "andreas_was_here_yesterday",       "'About that one I can say something, I heard he told you that he came here today, while in fact, he was playing with us yesterday.'"]
        , ["karl",      "jonas",    "jonas_likes_drinking_in_company",  "'This guy, I don't remember how many times I've seen him drinking here, but he rarely pays for his drinks - usually he sits with some random people, tells silly jokes, chit chats a bit and gets his drinks from these people completely for free. I've seen him perfecting this technique for months now...'"]
        , ["stephan",   "jonas",    "jonas_likes_drinking_in_company",  "'Ah yes, this funny boy. I drank with him a couple of times. He's the funniest person in the whole hotel and the best drinking buddy - that's why I buy him a drink from time to time.'"]
        , ["stephan",   "karl",     "karl_hides_cards_in_his_sleeve",   "'If I were you, I wouldn't play any card game with this guy. When I ordered a drink one day, I saw a playing card sliding out of his sleeve!'"]
        , ["urlich",    "karl",     "karl_cheats_at_poker",             "'Is no one around? Fine. If Sir really wants to know, I believe karl is cheating during our little poker games! I saw him once playing an ace of clubs - the exact same one I had in my hand!'"]
        , ["thomas",    "_WHY_HERE_",   "asked_thomas_why_is_he_here",  "'Dead man tell no tales'"]
        , ["giulia",    "_WHY_HERE_",   "asked_giulia_why_is_she_here", "'I came here with Thomas on vacations.'"]
        , ["andreas",   "_WHY_HERE_",   "asked_andreas_why_is_he_here", "'I came here as soon I heard that my brother is... he is dead!'"]
        , ["zoe",       "_WHY_HERE_",   "asked_zoe_why_is_she_here",    "'It's always nice to spend some free time in such a beatiful place, isn't it?'"]
        , ["amy",       "_WHY_HERE_",   "asked_amy_why_is_she_here",    "'I heard there are oppuritunities to get some cash here. Have you heard about it too by chance?'"]
        , ["stephan",   "_WHY_HERE_",   "asked_stephan_why_is_he_here", "'I wanted to talk to Thomas, I heard he's coming here for a couple of days. I won't be able to now...'"]
        , ["hermann",   "_WHY_HERE_",   "asked_hermann_why_is_he_here", "'Well, I live nearby and hunt deers for Thomas sometimes, like yesterday.''"]
        , ["jonas",     "_WHY_HERE_",   "asked_jonas_why_is_he_here",   "'I live nearby and I heard about the murder, so I thought I might gain some real life experience in this case. Did I mention that I'm studying law?'"]
        ]

    noticePeople state = state { message = ["You notice following people around you: "] ++ (map (\x -> fst x) (filter (\x -> snd x == (i_am_at state)) (people_at state))) }

    talk :: State -> String -> State
    talk state person = do
        let maybePerson = (List.find (\(x) -> person == fst x) (people_at state))
        (case maybePerson of
            Nothing -> state { message = ["You try talking to your new imaginary friend, but she/he isn't responding."] }
            Just(realPerson) -> if (snd realPerson == (i_am_at state)) then do
                                let newState = state { talking_to = person, message = [snd (Maybe.fromJust (List.find (\(x) -> person == fst x) people_descriptions))] }
                                newState { message = (message newState) ++ message (noticeTrinkets newState) }
                            else
                                state { message = ["You start to formulate your sentence towards " ++ person ++ ", when suddenly you realise, that he cannot hear you, for she/he isn't here."] })

    addFactToKnown :: State -> String -> String -> State
    addFactToKnown state object failMessage = 
        case (List.find (\(x) -> (talking_to state) == x!!0 && object == x!!1) describe) of
            Nothing -> state { message = [failMessage] }
            Just(description) -> state { known_facts = (description!!2):(known_facts state), message = [description!!3] ++ ["NEW FACT ADDED"] }

    internalTalkAbout :: State -> String -> String -> State
    internalTalkAbout state object failMessage = do
        let maybePerson = (List.find (\(x) -> (talking_to state) == fst x) (people_at state))
        (case maybePerson of
            Nothing -> state { message = ["You try talking to your new imaginary friend, but she/he isn't responding."] }
            Just(realPerson) -> if ((i_am_at state) == (snd realPerson)) then do
                    let maybePrerequisite = (List.find (\(x) -> (talking_to state) == fst (fst x) && object == snd (fst x)) prerequisites)
                    (case maybePrerequisite of
                        Nothing -> addFactToKnown state object failMessage
                        Just(realPrerequisite) ->
                            if not ((snd realPrerequisite) state) then
                                state { message = ["Maybe I know something about it, or maybe I don't..."] }
                            else
                                addFactToKnown state object failMessage)
                else
                    state { message = ["You start to formulate your sentence towards " ++ (talking_to state) ++ ", when suddenly you realise, that he cannot hear you, for she/he isn't here."] })

    tellAbout state fact =
        if elem fact (known_facts state) then
            internalTalkAbout state fact "'So... what I'm suppose to do with that information?'"
        else
            state { message = ["'And where did you get that from?'"] }

    askAbout state item =
        if elem item (holding state) || Maybe.isJust (List.find (\(x) -> (talking_to state) == snd x) (items_at state)) then
            internalTalkAbout state item ("'A " ++ item ++ ". What about it?'")
        else
            state { message = ["'I'd love to tell you something about " ++ item ++ ", but I'm afraid I don't know what it is...'"] }

    gossipAbout state person =
        internalTalkAbout state person "'Not much I can say about him/her.'"

    whyHere state =
        internalTalkAbout state "_WHY_HERE_" "'Well, I work here!'"
    
    accuse state person =
        if "hans" == (talking_to state) then
            if person == "zoe"
                && elem "zoe_knew_about_watch_changing_hands" (known_facts state)
                && elem "sleeping_pills" (holding state)
                && elem "zoe_did_not_know_about_giulia" (known_facts state)
                && elem "amy_passed_out" (known_facts state)
                && elem "zoe_left_right_after_amy" (known_facts state) then
                    state { message = ["Congratulations, you've won!"] }
            else
                state { message = ["'You've got to have more proof for such a bold statement, young man.'"] }
        else
            state { message = ["'You should tell that to hans, not me.'"] }