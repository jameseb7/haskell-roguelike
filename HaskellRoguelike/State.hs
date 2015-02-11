module HaskellRoguelike.State where

    import Control.Monad
    import Control.Monad.Identity
    import Control.Monad.State
    import Control.Monad.Random
    import Control.Applicative
    
    -- Type to contain identifiers for entities, which are passed around in
    -- the global state so that a unique identifier can be obtained whenever
    -- one is required.
    newtype EntityID = EntityID Int deriving (Eq, Ord, Show)

    newtype EntityIDGenT m a = 
        EntityIDGenT {runEntityIDGenT :: EntityID -> m (a, EntityID)}
    
    instance MonadTrans EntityIDGenT where
        lift m = EntityIDGenT $ \ eid -> m >>= (\ a -> return (a,eid))

    instance (Functor m) => Functor (EntityIDGenT m) where
        fmap f x = EntityIDGenT $ \ eid ->
                              fmap (\ (a,eid') -> (f a,eid')) $ 
                                   runEntityIDGenT x eid
                                                   
    instance (Functor m, Monad m) => Applicative (EntityIDGenT m) where
        pure = return
        (<*>) = ap

    instance (Monad m) => Monad (EntityIDGenT m) where
        return x = EntityIDGenT $ \ eid -> return (x,eid)
        x >>= f = EntityIDGenT $ \ eid -> do
                                (a,eid') <-runEntityIDGenT x eid
                                runEntityIDGenT (f a) eid'

    initialEntityID :: EntityID
    initialEntityID = EntityID 0
    
    newEntityID :: (Monad m) => EntityIDGenT m EntityID
    newEntityID = EntityIDGenT (\eid@(EntityID x) 
                                    -> return (eid, EntityID (x+1)))

    -- Monad transformer stack to hold all the state required by the roguelike
    type RLState s a =  StateT s (EntityIDGenT (RandT StdGen Identity)) a
