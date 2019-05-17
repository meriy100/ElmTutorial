module LazyList exposing (..)

type Trigger =
    Trigger
type alias Lazy a = Trigger -> a

type LazyList a =
    Cons (Lazy a) (Lazy (LazyList a))
    | LazyEmpty

new : a -> LazyList a
new x = Cons (\t -> x) (\t -> LazyEmpty)

new2 : a -> a -> LazyList a
new2 x1 x2 = Cons (\t -> x1) <| (\t -> new x2)

new3 : a -> a -> a -> LazyList a
new3 x1 x2 x3 = Cons (\t -> x1) <| \t -> (new2 x2 x3)

new4 x1 x2 x3 x4 = Cons (\t -> x1) <| \t -> (new3 x2 x3 x4)
new5 x1 x2 x3 x4 x5 = Cons (\t -> x1) <| \t -> (new4 x2 x3 x4 x5)
--
toString : (a -> String) -> LazyList a -> String
toString f xs =
    case xs of
        Cons y ys ->
            case ys Trigger of
                Cons _ _  ->
                    (f (y Trigger)) ++ ", " ++ toString f (ys Trigger)
                LazyEmpty ->
                    f (y Trigger)
        LazyEmpty ->
            ""
--
head : LazyList a -> Maybe a
head xs =
    case xs of
        Cons y _ ->
            Just (y Trigger)
        LazyEmpty ->
           Nothing

tail : LazyList a -> LazyList a
tail xs =
    case xs of
        Cons _ ys ->
            ys Trigger
        LazyEmpty ->
            LazyEmpty
map : (a -> b) -> LazyList a -> LazyList b
map f xs =
    case xs of
        Cons y ys ->
            Cons (\t -> f (y Trigger)) (\t -> map f (ys Trigger))
        LazyEmpty ->
            LazyEmpty
take : Int -> LazyList a -> LazyList a
take n xs =
    if n == 0 then
        LazyEmpty
    else
        case xs of
            Cons y ys ->
                Cons y (\t -> take (n - 1) (ys Trigger))
            LazyEmpty ->
                LazyEmpty

fill : a -> LazyList a
fill a =
    Cons (\t -> a)  (\t -> (fill a))

upto : Int -> LazyList Int
upto n =
    Cons (\t -> n) (\t -> upto (n + 1))

toFizzBuzz n =
   if modBy 15 n == 0 then
    "FizzBuzz"
   else if modBy 3 n == 0 then
    "Fizz"
   else if modBy 5 n == 0 then
    "Buzz"
   else
     String.fromInt n

fizzBuzz : LazyList String
fizzBuzz =
    upto 1 |> map toFizzBuzz
