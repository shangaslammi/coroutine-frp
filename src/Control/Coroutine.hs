
module Control.Coroutine where

import Prelude hiding (id, (.))

import Data.Functor
import Data.List (mapAccumL)
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad (liftM2)

newtype Coroutine i o = Coroutine { runC :: i -> (o, Coroutine i o) }

instance Functor (Coroutine i) where
    fmap f co = Coroutine $ \i ->
        let (o, co') = runC co i
        in (f o, fmap f co')

instance Applicative (Coroutine i) where
    pure x = Coroutine $ const (x, pure x)

    cof <*> cox = Coroutine $ \i ->
        let (f, cof') = runC cof i
            (x, cox') = runC cox i
        in (f x, cof' <*> cox')

instance Category Coroutine where
    id = Coroutine $ \i -> (i, id)

    cof . cog = Coroutine $ \i ->
        let (x, cog') = runC cog i
            (y, cof') = runC cof x
        in (y, cof' . cog')

instance Arrow Coroutine where
    arr f = Coroutine $ \i -> (f i, arr f)

    first co = Coroutine $ \(a,b) ->
        let (c, co') = runC co a
        in ((c,b), first co')

instance ArrowLoop Coroutine where
    loop co = Coroutine $ \b ->
        let ((c,d),co') = runC co (b,d)
        in (c, loop co')

instance ArrowChoice Coroutine where
    left co = Coroutine step where
        step ebd = case ebd of
            Left b  -> let (o, co') = runC co b in (Left o, left co')
            Right c -> (Right c, Coroutine step)

zipC :: (a -> b -> c) -> Coroutine (a,b) c
zipC = arr . uncurry

mapC :: Coroutine a b -> Coroutine [a] [b]
mapC co = Coroutine $ \as ->
    let (co', bs) = mapAccumL step co as
        step co a = (\(a,b)->(b,a)) $ runC co a
    in (bs, mapC co')

cycleC :: [b] -> Coroutine a b
cycleC = Coroutine . step . cycle where
    step (x:xs) _ = (x, Coroutine $ step xs)

joinCM :: Monad m => Coroutine (m a, m b) (m (a,b))
joinCM = zipC $ liftM2 (,)

joinCA :: Applicative p => Coroutine (p a, p b) (p (a,b))
joinCA = zipC $ liftA2 (,)

evalList :: Coroutine i o -> [i] -> [o]
evalList _  []     = []
evalList co (x:xs) = o:evalList co' xs
    where (o, co') = runC co x
