module Person where
    import Prelude
    import qualified Data.Maybe as Maybe
    import qualified Data.List as List

    import State

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

    talk state person = do
        let maybePerson = (List.find (\(x) -> person == fst x) (people_at state))
        case maybePerson of
            Nothing -> state { message = ["You try talking to your new imaginary friend, but she/he isn't responding."] }
            Just(realPerson) -> if (snd realPerson == (i_am_at state)) then
                                state { talking_to = person, message = [snd (Maybe.fromJust (List.find (\(x) -> person == fst x) people_descriptions))] }
                            else
                                state { message = ["You start to formulate your sentence towards " ++ person ++ ", when suddenly you realise, that he cannot hear you, for she/he isn't here."] }

    noticePeople state = state { message = ["You notice following people around you: "] ++ (map (\x -> fst x) (filter (\x -> snd x == (i_am_at state)) (people_at state))) }
