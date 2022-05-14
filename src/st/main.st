"TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek."

Object subclass: Game [
    | isGameOver |

    introductionText := #(
        'Daily life in the alpine hotel was disrupted by the barking of the Promyczek. The wife, which came after that to the room, saw her husband on the floor. She started screaming and woke up everyone who stayed in the hotel. The hotel owner immediately calls you to solve that riddle. Who is the murderer? You do not know. Yet...'
    ).

    helpText := #(
        'Available commands are:'
        ''
        'help          -- to see these instructions.'
        'quit          -- to end the game and quit.'
        ''
    ).
 
    Game class >> new [
        | game |
        game := super new.
        game init.
        ^ game
    ]

    init [
        isGameOver := false
    ]

    write: array [
        array do: [ :string | Transcript show: string; cr ]
    ]

    help [
        self write: helpText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    run [
        | cmd isUnknown |

        self write: introductionText.
        self help.

        [isGameOver] whileFalse: [

            isUnknown := true.
            cmd := self readCommand.

            cmd = 'help' ifTrue: [
                self help.
                isUnknown := false
            ].

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].
                
            isUnknown ifTrue: [
                self write: #('Unknown command.' '')
            ]
        ]
    ]
]

Game new run.
