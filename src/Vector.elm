module Vector exposing (..)


type alias Vector =
    { x : Float
    , y : Float
    }


pair2vect : ( Float, Float ) -> Vector
pair2vect ( x, y ) =
    Vector x y


vect2pair : Vector -> ( Float, Float )
vect2pair { x, y } =
    ( x, y )


lift0 : Float -> Vector
lift0 x =
    Vector x x


lift1 : (Float -> Float) -> Vector -> Vector
lift1 f { x, y } =
    Vector (f x) (f y)


lift2 : (Float -> Float -> Float) -> Vector -> Vector -> Vector
lift2 f v1 v2 =
    Vector (f v1.x v2.x) (f v1.y v2.y)


vectAdd : Vector -> Vector -> Vector
vectAdd =
    lift2 (+)


vectSub : Vector -> Vector -> Vector
vectSub =
    lift2 (-)


vectScale : Float -> Vector -> Vector
vectScale s =
    lift1 ((*) s)


vectRotate : Float -> Vector -> Vector
vectRotate phi { x, y } =
    let
        ( r, p ) =
            toPolar ( x, y )
    in
        fromPolar ( r, p + phi ) |> pair2vect
