module HaskellRoguelike.State 
    (
     EntityID, EntityIDGenT,
     runEntityIDGenT, initialEntityID, newEntityID,
     RLState,
     evalRLState, runRLState, genState
    )
    where

    import Control.Monad
    import Control.Monad.Identity
    import Control.Monad.State
    import Control.Monad.Random
     
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

    instance (MonadRandom m) => MonadRandom (EntityIDGenT m) where
        getRandom = lift getRandom
        getRandoms = lift getRandoms
        getRandomR = lift . getRandomR
        getRandomRs = lift . getRandomRs

    evalRLState :: RLState s a -> s -> EntityID -> StdGen -> a
    evalRLState rls s eid g = a
        where (a,_) = evalRand a' g
              a' = runEntityIDGenT a'' eid
              a'' = evalStateT rls s

    runRLState :: RLState s a -> s -> EntityID -> StdGen
               -> (a, s, EntityID, StdGen)
    runRLState rls s eid g = (a, s', eid', g')
        where a'' = runStateT rls s
              a' = runEntityIDGenT a'' eid
              (((a, s'), eid'), g') = runRand a' g

    genState :: EntityIDGenT (Rand StdGen) s -> IO (s, EntityID, StdGen)
    genState rls = do g <- newStdGen
                      let a = runEntityIDGenT rls initialEntityID
                      let ((s, eid), g') = runRand a g
                      return (s, eid, g')
