{-# LANGUAGE BangPatterns #-}

module Control.Coroutine where

import Prelude hiding (id, (.))

import Data.Monoid
import Data.List (mapAccumL)
import Data.IORef
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad (liftM2)

newtype Coroutine i o = Coroutine { runC :: i -> (o, Coroutine i o) }

instance Functor (Coroutine i) where
    fmap f co = Coroutine $ \i ->
        let (o, co') = runC co i
        in (f o, fmap f co')

    {-# INLINE fmap #-}

instance Applicative (Coroutine i) where
    pure x = Coroutine $ const (x, pure x)

    cof <*> cox = Coroutine $ \i ->
        let (f, cof') = runC cof i
            (x, cox') = runC cox i
        in (f x, cof' <*> cox')

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monoid o => Monoid (Coroutine i o) where
    mempty  = pure mempty
    mappend = (<++>)

instance Category Coroutine where
    id = Coroutine $ \i -> (i, id)

    a . b = Coroutine $ step a b where
        step !cof !cog i = (y, Coroutine $ step cof' cog') where
            (x, cog') = runC cog i
            (y, cof') = runC cof x

    {-# INLINE id #-}
    {-# INLINE (.) #-}

instance Arrow Coroutine where
    arr f = Coroutine step where
        step i = (f i, Coroutine step)

    first = Coroutine . step where
        step !co (a,b) = ((c,b), Coroutine $ step co') where
            (c, co') = runC co a

    second = Coroutine . step where
        step !co (a,b) = ((a,c), Coroutine $ step co') where
            (c, co') = runC co b

    ca &&& cb = Coroutine $ step ca cb where
        step !cof !cog a = ((b, c), Coroutine $ step cof' cog') where
            (b, cof') = runC cof a
            (c, cog') = runC cog a

    {-# INLINE arr #-}
    {-# INLINE first #-}
    {-# INLINE second #-}
    {-# INLINE (&&&) #-}

instance ArrowLoop Coroutine where
    loop co = Coroutine $ \b ->
        let ((c,d),co') = runC co (b,d)
        in (c, loop co')

instance ArrowChoice Coroutine where
    left co = Coroutine step where
        step ebd = case ebd of
            Left b  -> let (o, co') = runC co b in (Left o, left co')
            Right c -> (Right c, Coroutine step)

(<++>) :: Monoid o => Coroutine i o -> Coroutine i o -> Coroutine i o
(<++>) = liftA2 mappend

zipC :: (a -> b -> c) -> Coroutine (a, b) c
zipC = arr . uncurry

mapC :: Coroutine a b -> Coroutine [a] [b]
mapC co = Coroutine $ \as ->
    let (co', bs) = mapAccumL step co as
        step c x = (\(a,b)->(b,a)) $ runC c x
    in (bs, mapC co')

filterC :: (a -> Bool) -> b -> Coroutine a b -> Coroutine a b
filterC p initial = Coroutine . step initial where
    step b co a = (b', Coroutine $ step b' co') where
        (b', co') = if p a
            then runC co a
            else (b, co)

filterOutC :: (b -> Bool) -> Coroutine a b -> Coroutine a b
filterOutC p = Coroutine . step where
    step co a = let (b, co') = runC co a
        in if (p b)
            then (b, Coroutine $ step co')
            else step co' a

cycleC :: [b] -> Coroutine a b
cycleC = Coroutine . step . cycle where
    step (x:xs) _ = (x, Coroutine $ step xs)
    step _      _ = error "impossible"  -- cycle never returns an empty list

updateC :: a -> Coroutine (a -> a) a
updateC = Coroutine . step where
    step a f = (a', Coroutine $ step a') where
        a' = f a

zipCM :: Monad m => Coroutine (m a, m b) (m (a, b))
zipCM = zipC $ liftM2 (,)

zipCA :: Applicative p => Coroutine (p a, p b) (p (a,b))
zipCA = zipC $ liftA2 (,)

evalList :: Coroutine i o -> [i] -> [o]
evalList _  []     = []
evalList co (x:xs) = o:evalList co' xs
    where (o, co') = runC co x

wrapIO :: Coroutine a b -> IO (a -> IO b)
wrapIO initial = do
    ref <- newIORef initial
    return $ \a -> do
        co <- readIORef ref
        let (b, co') = runC co a
        writeIORef ref co'
        return b
