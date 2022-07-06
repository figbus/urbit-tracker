module Dev exposing (main)

import Main exposing (Model, Msg, StartFlag(..))
import Urbit.Encoding.Phonemic


main : Program Int Model Msg
main =
    Main.toMain <|
        Dev
            (Urbit.Encoding.Phonemic.fromPatp "~zod")
            "http://localhost:80"
            "lidlut-tabwed-pillex-ridrup"
