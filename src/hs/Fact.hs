module Fact where
    import State

    journal state = state { message = "You know these facts: " : known_facts state }