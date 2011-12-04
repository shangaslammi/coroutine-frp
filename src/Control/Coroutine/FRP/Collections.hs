
module Control.Coroutine.FRP.Collections where

import Control.Coroutine
import Control.Coroutine.FRP
import Control.Arrow

import Data.List (foldl')
import qualified Data.IntMap as IntMap

newtype RecvID = RecvID { unRecvID :: Int }

type TEvent a = [Tagged a]
type Tagged a = (RecvID, a)

type Item i a         = Coroutine i (Maybe a)
type Receiver i e a   = Coroutine (i, Event e) (Maybe a)
type Sender i e a     = Coroutine i (Maybe a, Event e)
type RecvSend i r s a = Coroutine (i, Event r) (Maybe a, Event s)

untag :: [Tagged a] -> [a]
untag = map snd

collection  :: [Item i a]
            -> Coroutine (i, Event (Item i a)) [a]
collection = Coroutine . step where
    step col (i, ev) = (objs, cont) where
        (objs, conts) = unzip $ runConts $ ev ++ col
        cont = Coroutine $ step conts
        runConts = foldr prune [] . map (\c -> runC c i)

    prune x a = case x of
        (Nothing, _) -> a
        (Just x,  c) -> (x,c):a

receivers   :: [Receiver i e a]
            -> Coroutine (i, (Event (Receiver i e a), TEvent e)) [Tagged a]
receivers = Coroutine . initCol where
    initCol = uncurry step . foldl' add (0, IntMap.empty)

    step cid col (i, (e, te)) = (objs, cont) where
        te' = IntMap.fromListWith (++) $ map (unRecvID *** return) te
        (cid', col')  = foldl' add (cid, col) e
        (objs, conts) = foldr process ([],[]) $ IntMap.toAscList col'
        cont = Coroutine $ step cid' $ IntMap.fromDistinctAscList conts
        process (rid, c) (os,cs) = case runC c (i, ev) of
            (Nothing, _) -> (os, cs)
            (Just o, c') -> ((RecvID rid,o):os,(rid,c'):cs)
            where ev = IntMap.findWithDefault [] rid te'

    add (cid, col) x = (cid+1, IntMap.insert cid x col)

senders :: [Sender i e a]
        -> Coroutine (i, Event (Sender i e a)) ([a], Event e)
senders = Coroutine . step where
    step col (i, ev) = ((objs, evs), cont) where
        (objs, evs, conts) = foldr process ([],[],[]) $ ev ++ col
        process c (os,es,cs) = case runC c i of
            ((Nothing, ev), _) -> (os,ev++es,cs)
            ((Just o, ev), c') -> (o:os,ev++es,c':cs)
        cont = Coroutine $ step conts

recvSenders :: [RecvSend i r s a]
            -> Coroutine (i, (Event (RecvSend i r s a), TEvent r)) ([Tagged a], Event s)
recvSenders = Coroutine . initCol where
    initCol = uncurry step . foldl' add (0, IntMap.empty)

    step cid col (i, (e, te)) = ((objs, evs), cont) where
        te' = IntMap.fromListWith (++) $ map (unRecvID *** return) te
        (cid', col')  = foldl' add (cid, col) e
        (objs, conts, evs) = foldr process ([],[],[]) $ IntMap.toAscList col'
        cont = Coroutine $ step cid' $ IntMap.fromDistinctAscList conts
        process (rid, c) (os,cs,es) = case runC c (i, ev) of
            ((Nothing, e), _) -> (os, cs, e ++ es)
            ((Just o, e), c') -> ((RecvID rid,o):os,(rid,c'):cs, e ++ es)
            where ev = IntMap.findWithDefault [] rid te'

    add (cid, col) x = (cid+1, IntMap.insert cid x col)
