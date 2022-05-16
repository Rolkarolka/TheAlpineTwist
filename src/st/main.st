"TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek."

Object subclass: Locations [
    | paths people items currentlyAt |
    Locations class >> new [
        | map |
        map := super new.
        map init.
        ^ map
    ]

    init [
        currentlyAt := 'room_of_thomas_and_giulia'.

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

        people := Dictionary new.
        people at: 'room_of_thomas_and_giulia' put: #('thomas' 'giulia' 'andreas' '').
        people at: 'room_of_zoe' put: #('zoe' '').
        people at: 'bar' put: #('karl' 'amy' 'stephan' '').
        people at: 'corridor' put: #('jurgen' 'hilda' '').
        people at: 'kitchen' put: #('theodor' '').
        people at: 'reception' put: #('hans' 'hermann' '').
        people at: 'hotel_entrance' put: #('jonas' 'urlich' '').

        items := Dictionary new.
        items at: 'room_of_thomas_and_giulia' put: #('watch' 'thomas_journal' 'cigarette_light' '').
        items at: 'room_of_zoe' put: #('sleep_mask' 'sleeping_pills' 'cup' '').
        items at: 'karl' put: #('clubs_symbol' '').
        items at: 'bar' put: #('glass' '').
        items at: 'hilda' put: #('brooch' '').
        items at: 'corridor' put: #('cleaning_stuff' 'cutlery_tray' '').
        items at: 'reception' put: #('guest_book' 'telephone' 'ball' '').
        items at: 'hermann' put: #('hunting_weapon' '').
        items at: 'urlich' put: #('gilded_epaulettes' '').
        items at: 'hotel_entrance' put: #('bush' '').
        items at: 'hunters_shaque' put: #('bullets' 'knife_scabbard' 'blooded_knife' '').
        items at: 'kitchen' put: #('deer' 'broth' '').
    ]

    go: direction [
        | message |
        currentlyAt := (paths at: (currentlyAt, direction) ifAbsent: [currentlyAt]).

        message := Array new: 2.
        message at: 1 put: 'Currently at: ', currentlyAt.
        message at: 2 put: ''.
        ^ message.
    ]

    look [
        ^ (people at: currentlyAt ifAbsent: [#('')])
    ]

    notice [
        ^ (items at: currentlyAt ifAbsent: [#('')])
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
        'w a s d       -- to go in that direction.'
        'look / l      -- to look at people around you.'
        'notice / n    -- to look at people around you.'
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

            cmd = 'look' | cmd = 'l' ifTrue: [
                self write: (locations look).
                isUnknown := false
            ].

            cmd = 'notice' | cmd = 'n' ifTrue: [
                self write: (locations notice).
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
