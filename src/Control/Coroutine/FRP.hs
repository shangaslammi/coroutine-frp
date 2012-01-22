{-# LANGUAGE BangPatterns #-}

module Control.Coroutine.FRP where

import qualified Control.Category as C
import Control.Applicative (pure)
import Control.Arrow
import Data.List (foldl')

import Control.Coroutine
import qualified Data.IntMap as IntMap

type Event a = [a]

edge :: Eq a => Coroutine a (Event a)
edge = Coroutine $ \i -> ([], step i) where
    step old = Coroutine $ \i ->
        if old == i
            then ([],  step i)
            else ([i], step i)

watch :: (a -> Bool) -> Coroutine a (Event a)
watch f = Coroutine $ \i ->
    if f i
        then ([i], watch f)
        else ([], watch f)

scan :: (a -> b -> a) -> a -> Coroutine b a
scan f i = Coroutine $ step i where
    step !a b = let a' = f a b in (a', Coroutine $ step a')

{-# INLINE scan #-}

withPrevious :: a -> Coroutine a (a,a)
withPrevious first = Coroutine $ \i -> ((i, first), step i) where
    step old = Coroutine $ \i -> ((i, old), step i)

withPrevious' :: Coroutine a (a,a)
withPrevious' = Coroutine $ \i -> ((i,i), step i) where
    step old = Coroutine $ \i -> ((i, old), step i)

delay :: a -> Coroutine a a
delay a = withPrevious a >>> arr snd

every :: Int -> e -> Coroutine a (Event e)
every nth e = Coroutine $ step 0 where
    step 0 _ = ([e], Coroutine $ step nth)
    step n _ = ([], Coroutine  $ step $ n-1)

countUp :: Int -> Coroutine a Int
countUp = Coroutine . step where
    step n _ = (n', Coroutine $ step n') where n' = n + 1

countDown :: Int -> Coroutine a Int
countDown = Coroutine . step where
    step n _ = (n', Coroutine $ step n') where n' = n - 1

integrate :: Num a => a -> Coroutine a a
integrate = scan (+)

derivate :: Num a => Coroutine a a
derivate = withPrevious 0 >>> zipC (-)

scanE :: (a -> e -> a) -> a -> Coroutine (Event e) a
scanE f i = Coroutine $ step i where
    step a [] = (a, Coroutine $ step a)
    step !a e = let a' = foldl' f a e in (a', Coroutine $ step a')

mapE :: (e -> e') -> Coroutine (Event e) (Event e')
mapE = arr . map

tagE :: Coroutine (a, Event e) (Event a)
tagE = arr $ \(a, ev) -> map (const a) ev

concatMapE :: (e -> Event e') -> Coroutine (Event e) (Event e')
concatMapE = arr . concatMap

filterE :: (e -> Bool) -> Coroutine (Event e) (Event e)
filterE = arr . filter

mergeE :: Coroutine (Event e, Event e) (Event e)
mergeE = zipC (++)

constE :: e -> Coroutine (Event e') (Event e)
constE = mapE . const

updateE :: a -> Coroutine (Event (a -> a)) a
updateE = Coroutine . step where
    step a fs = (a', Coroutine $ step a') where
        a' = foldl' (flip ($)) a fs

stepE :: a -> Coroutine (Event a) a
stepE a = Coroutine $ \ev ->
    let a' = last (a:ev)
    in (a', stepE a')

skipE :: Int -> Coroutine (Event e) (Event e)
skipE n = Coroutine $ step n where
    step 0 ev = (ev, C.id)
    step n ev = ([], Coroutine $ step $ n-1)

onceE :: [a] -> Coroutine i (Event a)
onceE events = onceThen events $ pure []

onceThen :: [a] -> Coroutine i (Event a) -> Coroutine i (Event a)
onceThen events co = Coroutine $ \_ -> (events, co)

restartWhen :: Coroutine a b -> Coroutine (a, Event e) b
restartWhen co = Coroutine $ step co where
    step c (i, ev) = (o, Coroutine cont) where
        (o, c') = runC c i
        cont
            | null ev   = step c'
            | otherwise = step co

switchE :: Coroutine a b -> Coroutine (a, Event (Coroutine a b)) b
switchE = switchWith id

switchWith :: (e -> Coroutine a b) -> Coroutine a b -> Coroutine (a, Event e) b
switchWith switch co  = Coroutine $ step1 co where
    step1 co (a, []) = (b, Coroutine $ step1 co') where
        (b, co') = runC co a
    step1 _ (a, ev) = (b, Coroutine $ step1 co') where
        co = switch (last ev)
        (b, co') = runC co a

delayE :: Int -> Coroutine (Event e) (Event e)
delayE delay = arr (const delay) &&& C.id >>> delayEn

delayEn :: Coroutine (Int, Event e) (Event e)
delayEn = Coroutine $ step 0 IntMap.empty where
    step !cur !buffer (delay, ev) = (ev', Coroutine $ step (cur+1) buffer'') where
        ev' = IntMap.findWithDefault [] cur buffer'
        buffer'
            | null ev   = buffer
            | otherwise = IntMap.insertWith (++) (cur+delay) ev buffer
        buffer'' = IntMap.delete cur buffer'
