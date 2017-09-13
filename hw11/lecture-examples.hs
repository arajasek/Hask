import Control.Applicative

-- Defintion of liftA2 from Lecture 10
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 h fa fb = (h `fmap` fa) <*> fb

-- ============================================================================
-- (*>) is already a thing, call this starket
starket :: Applicative f => f a -> f b -> f b
starket = liftA2 (const id) -- note that (const id) :: a -> b -> b, exactly what we want

-- ****************************************************************************
-- f = Maybe: If both arguments are not Nothing, returns the second argument; else returns Nothing
-- Nothing `starket` (Just 42)
-- => ((const id) `fmap` Nothing) <*> (Just 42)
-- => ((Just (const id)) <*> Nothing) <*> (Just 42)
-- => Nothing <*> (Just 42)
-- => Nothing

-- (Just 1) `starket` (Just 2)
-- => ((const id) `fmap` (Just 1)) <*> (Just 2)
-- => ((Just (const id)) <*> (Just 1)) <*> (Just 2)
-- => (Just (const id 1)) <*> (Just 2)
-- => (Just id) <*> (Just 2) -- by definition of const
-- => (Just (id 2))
-- => (Just 2)

-- ****************************************************************************
-- f = []: For two lists x, y, we'll get y repeated |x| times
-- [1, 2] `starket` [3, 4, 5]
-- => ((const id) `fmap` [1, 2]) <*> [3, 4, 5]
-- => ([const id] <*> [1, 2]) <*> [3, 4, 5]
-- => (map (const id) [1, 2]) <*> [3, 4, 5]
-- => [(const id 1), (const id 2)] <*> [3, 4, 5]
-- => [id, id] <*> [3, 4, 5]
-- => (map id [3, 4, 5]) ++ ([id] <*> [3, 4, 5])
-- => [3, 4, 5] ++ (map id [3, 4, 5]) ++ ([] <*> [3, 4, 5])
-- => [3, 4, 5] ++ [3, 4, 5] ++ []
-- => [3, 4, 5, 3, 4, 5]

-- ****************************************************************************
-- f = ZipList: Just returns the second list, unmapped by the fns in the first list,
-- since we effectively convert those fns to (const id f) === id.

-- ****************************************************************************
-- f = IO: Runs two IO sequences, but only returns the result of the second one.

-- ****************************************************************************
-- f = Parser: Runs two parsers, each consuming consecutive input, but only returns the result
-- of running the second one.

-- ============================================================================
mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA _ [] = pure []
mapA h (a:as) = liftA2 (:) (h a) (mapA h as) -- map the first element, to get an Applicative value, f b
                                             -- then 'applicatively' combine that with mapAing over the
                                             -- rest of the list, using cons as the combinator to put
                                             -- everything into a list (_inside_ the context).

-- ============================================================================
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (a:as) = liftA2 (:) a (sequenceA as) -- very similar to above; can skip the intermediate step of
                                               -- building the Applicative, since that's given as input list.

-- ============================================================================
replicateA :: Applicative f => Int -> f a -> f [a]
replicateA 0 _ = pure []
replicateA k elem = sequenceA (replicate k elem)