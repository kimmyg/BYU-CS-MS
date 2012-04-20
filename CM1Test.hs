module Main where
import CM1

{--

fact :: Int -> CM1 String ([String], Int)
fact 0 = do
  marks <- ccm
  return (marks, 1)

fact n = do
  (marks, acc) <- wcm (show n) (fact (n - 1));
  return (marks, (n * acc))

--}

-- test :: CM1 String [Maybe String]


-- test = wcm "v1" $ do
--   ccm
-- ok, is [Just "v1"]

-- test = wcm "v1" $ do
--   wcm "v2" $ do
--     ccm
-- ok, is [Just "v2"]

-- test = wcm "v1" $ do
--   ms <- ccm
--   return ms
-- is [Nothing, Just "v1"]

-- test = wcm "v1" $ do
--   x <- return 1
--   ccm
-- is [Just "v1"]

test = wcm "1" $ do
  (x, ms) <- wcm "2" $ do
    ms <- ccm
    return (1, ms)
  return (1+x, ms)

-- test = wcm "v1" $ do
--   wcm "v2" $ do
--     ms <- ccm
--     return ms
-- is [Nothing, Just "v2"]

{--

(wcm "1"
  (+ 1 (wcm "2"
     (begin
       (display ccm)
       1)))

wcm "1" $ do
  x <- return 1
  wcm "2" $ do
    y <- return 1
    ms <- ccm
    return (x+y, ms)

--}

ucm :: (Maybe m -> m) -> CM1 m a -> CM1 m a
ucm t m = do
  ms <- ccm
  case ms of
    []     -> wcm (t Nothing) m
    (m':_) -> do
      wcm (t Nothing) $ do
        (m'':_) <- ccm
        if m'' == (t Nothing)
          wcm (t Nothing) m
        else
          wcm (
          
main = print $ runCM1 $ wcm "1" $ do
  ms <- ccm
  case ms of
    []    -> return 1
    (m:_) -> 
  ms <- ccm
  return ms
