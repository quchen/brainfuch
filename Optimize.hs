module Optimize (optimize) where


import Types


-- | Optimizes a complete Brainfuck programuntil the code doesn't change
--   anymore.
--
--   Note: don't apply this to subroutines recursively, as it will drop loops at
--   the code's beginning.
optimize :: BrainfuckSource -> BrainfuckSource
optimize = iterateUntilFix $ simplify . trimBeginningLoop



-- | Apply endomorphism until fixed point is reached
iterateUntilFix :: (Eq a) => (a -> a) -> a -> a
iterateUntilFix f x | x == fx   =  x
                    | otherwise = iterateUntilFix f fx
                    where fx = f x


-- | Runs one simplification pass over the source.
simplify :: BrainfuckSource -> BrainfuckSource
simplify (BFSource xs) = BFSource $ foldr simplifyStep (`commit` []) xs Nothing
                                                -- ^ commits remaining cache


-- | Used as a folding function. Walks over the list and retains a cache of the
--   last instruction. If a new instruction matches the old one the two are
--   combined, otherwise the cache is committed and the new action is cached.
--
--   Applies the following simplifications:
--     - Combine multiple consecutive Add, Move and Print respectively
--     - Drops all but the first consecutive loop

simplifyStep :: BrainfuckCommand                               -- ^ New command
             -> (Maybe BrainfuckCommand -> [BrainfuckCommand]) -- ^ Accumulator
             ->  Maybe BrainfuckCommand                        -- ^ Cache
             -> [BrainfuckCommand]                           -- ^ Committed code



-- Combine multiple 'Add'
simplifyStep     (Add n) acc (Just (Add m)) = acc (Just . Add $! n+m)
simplifyStep add@(Add _) acc cache          = commit cache $ acc (Just add)

-- Combine multiple 'Move'
simplifyStep      (Move n) acc (Just (Move m)) = acc (Just . Move $! n+m)
simplifyStep move@(Move _) acc cache           = commit cache $ acc (Just move)

-- Combine multiple 'Print'
simplifyStep     (Print n) acc (Just (Print m)) = acc (Just . Print $! n+m)
simplifyStep put@(Print _) acc cache            = commit cache $ acc (Just put)

-- Read is not opzimized
simplifyStep Read acc cache = commit cache $ acc (Just Read)

-- Ignore all but the first successive loop
simplifyStep      (Loop _) acc cache@(Just (Loop _)) = acc cache
simplifyStep loop@(Loop _) acc cache = commit cache $ acc (Just loop)


-- | Commits a command. This is a synonym for (:), but will drop 'Add 0',
--   'Move 0' and recursively optimize loops.
commit ::  Maybe BrainfuckCommand
       -> [BrainfuckCommand]
       -> [BrainfuckCommand]
commit Nothing            = id
commit (Just (Add  0   )) = id
commit (Just (Move 0   )) = id
commit (Just (Loop body)) = (Loop (simplify body) :)
commit (Just cache      ) = (cache :) -- Happy closing parenthesis



-- | Drops loops at the beginning of the source as the pivot is zero and the
--   loop won't ever be entered.
trimBeginningLoop (SFSource xs) = SFSource $ dropWhile isLoop xs
      where isLoop (Loop _) = True
            isLoop _        = False