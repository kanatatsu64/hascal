{-# LANGUAGE FlexibleContexts #-}

module Debug (
    dump,
    peek
) where

import Pipes ( Pipe, Consumer, await, yield, (>->), MonadIO, liftIO )

end :: Monad m => Consumer a m r
end = await >> end

dump :: (Show a, MonadIO m) => String -> Consumer a m r
dump tag = peek tag >-> end

peek :: (Show a, MonadIO m) => String -> Pipe a a m r
peek tag = do
    t <- await
    liftIO $ print $ tag <> ": " <> show t
    yield t
    peek tag