"TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek."

Object subclass: Locations [
    | paths currentlyAt |
    Locations class >> new [
        | map |
        map := super new.
        map init.
        ^ map
    ]

    init [
        paths := Dictionary new.
        paths at: ('room_of_thomas_and_giulia', 'west') put: 'corridor'.
        paths at: ('corridor', 'east') put: 'room_of_thomas_and_giulia'.
        paths at: ('corridor', 'west') put: 'room_of_zoe'.
        paths at: ('corridor', 'south') put: 'reception'.
        paths at: ('room_of_zoe', 'east') put: 'corridor'.
        paths at: ('reception', 'north') put: 'corridor'.
        paths at: ('reception', 'west') put: 'bar'.
        paths at: ('reception', 'south') put: 'hotel_entrance'.
        paths at: ('bar', 'east') put: 'reception'.
        paths at: ('bar', 'south') put: 'kitchen'.
        paths at: ('hotel_entrance', 'north') put: 'reception'.
        paths at: ('hotel_entrance', 'south') put: 'hunters_shaque'.
        paths at: ('kitchen', 'north') put: 'bar'.
        paths at: ('kitchen', 'east') put: 'hunters_shaque'.
        paths at: ('hunters_shaque', 'north') put: 'hotel_entrance'.
        paths at: ('hunters_shaque', 'west') put: 'kitchen'.
        currentlyAt := 'room_of_thomas_and_giulia'.
    ]

    go: direction [
        | message |
        currentlyAt := (paths at: (currentlyAt, direction) ifAbsent: [currentlyAt]).

        message := Array new: 2.
        message at: 1 put: 'Currently at: ', currentlyAt.
        message at: 2 put: ''.
        ^ message.
    ]

    at [
        ^ currentlyAt
    ]
]

Object subclass: Game [
    | isGameOver locations |

    introductionText := #(
        'Daily life in the alpine hotel was disrupted by the barking of the Promyczek. The wife, which came after that to the room, saw her husband on the floor. She started screaming and woke up everyone who stayed in the hotel. The hotel owner immediately calls you to solve that riddle. Who is the murderer? You do not know. Yet...'
    ).

    helpText := #(
        'Available commands are:'
        ''
        'help          -- to see this message again.'
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
        isGameOver := false.
        locations := Locations new.
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

            cmd = 'w' ifTrue: [
                self write: (locations go: 'north').
                isUnknown := false
            ].

            cmd = 'd' ifTrue: [
                self write: (locations go: 'east').
                isUnknown := false
            ].

            cmd = 'a' ifTrue: [
                self write: (locations go: 'west').
                isUnknown := false
            ].

            cmd = 's' ifTrue: [
                self write: (locations go: 'south').
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
