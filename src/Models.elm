module Models exposing(..)

    -- tuple with type alias
type alias ImageUrl = String
type alias ProductUrl = String
type alias Card = (ImageUrl, ProductUrl)

type alias Model = List Card 

initialModel : Model
initialModel = 
    [("ImageUrl1","ProductUrl1")
    ,("ImageUrl2","ProductUrl2")
    ,("ImageUrl3","ProductUrl3")
    ,("ImageUrl4","ProductUrl4")
    ]





