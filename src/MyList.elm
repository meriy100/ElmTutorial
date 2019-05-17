module MyList exposing (..)

type MyList a =
    Cons a (MyList a)
    | Empty

new : a -> MyList a
new x = Cons x Empty

new2 : a -> a -> MyList a
new2 x1 x2 = Cons x1 <| new x2

new3 : a -> a -> a -> MyList a
new3 x1 x2 x3 = Cons x1 <| new2 x2 x3

new4 x1 x2 x3 x4 = Cons x1 <| new3 x2 x3 x4
new5 x1 x2 x3 x4 x5 = Cons x1 <| new4 x2 x3 x4 x5

toString : (a -> String) -> MyList a -> String
toString f xs =
    case xs of
        Cons y Empty ->
            (f y)
        Cons y ys ->
            (f y) ++ ", " ++ toString f ys
        Empty ->
            ""

head : MyList a -> Maybe a
head xs =
    case xs of
        Cons y _ ->
            Just y
        Empty ->
           Nothing

tail : MyList a -> MyList a
tail xs =
    case xs of
        Cons _ ys ->
            ys
        Empty ->
            Empty
map : (a -> b) -> MyList a -> MyList b
map f xs =
    case xs of
        Cons y ys ->
            Cons (f y) (map f ys)
        Empty ->
            Empty
take : Int -> MyList a -> MyList a
take n xs =
    if n == 0 then
        Empty
    else
        case xs of
            Cons y ys ->
                Cons y (take (n - 1) ys)
            Empty ->
                Empty

fill : a -> MyList a
fill a =
    Cons a (fill a)

