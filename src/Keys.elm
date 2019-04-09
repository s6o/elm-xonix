module Keys
    exposing
        ( KeyName(..)
        , KeyState(..)
        )


type KeyName
    = KeyArrowDown
    | KeyArrowLeft
    | KeyArrowRight
    | KeyArrowUp


type KeyState
    = KeyNotPressed
    | KeyPressed
