-- CMM - Continuation Mark Monad

type Key = String
type Value = String
type CMM a = (a, [(Key,Value)])

cmmReturn :: a -> CMM a
cmmReturn x = (x, [])
we were passed the stack of continuation marks
we set it in the frame and pass it along...?

cmmBind :: CMM a -> (a -> CMM b) -> CMM b
cmmBind (x, cms) f = let (y, new_cms) = f x in (y, new_cms++cms)
[Frame]

wcm :: CMM a -> Key -> Value -> (CMM a -> CMM b) -> CMM b
wcm (x, cms) key value f = f (x, ((key, value):cms))

ccm :: CMM a -> Key -> [Value]
ccm (_, cms) key = [ value | (key', value) <- cms, key' == key ]
factorial 0 frames = (1, frames)
factorial n f:fs = 

--instance Monad (CMM a) where
--return x = cmmReturn x
--x >>= f = cmmBind x f
= n * factorial []:frames (n - 1)

main = print (wcm (cmmReturn 5) "type" "number")

f :: CCM Int -> ...
f (x, cms) = let types = ccm cms "type" in 
wcm f:fs key value <something> = <something> (frameSet f key value)

factorial ( ... frames ... n ... ) {
	wcm frames "n" (show n) (n * factorial []:frames (n - 1))

factorial :: Int -> [Frame] -> Int
factorial 0 fs = 1
factorial n fs = wcm fs "n" (show n) (\fs -> n * factorial []:fs (n - 1))

fac2 :: Int -> Int -> [Frame] -> Int
fac2 0 acc fs = acc
fac2 n acc fs = wcm fs "n" (show n) (\fs -> fac2 (n - 1) (acc * n) fs)

wcm uses only the frames it is given.

we must manually add a frame to the frames when we generate a recursive call

don't return the frames, each level only knows about the ones below it

wcm :: [Frame] -> Key -> Value -> ( [Frame] -> a ) -> a
wcm [] key value g = g [Frame [(key, value)]]
wcm f:fs key value g = g (frameSet f key value):fs

ccm :: [Frame] -> Key -> [Value]
ccm [] key = []
ccm f:fs key = map (\(Just x) -> x) $ filter (\x -> x /= Nothing) (frameGet f key):(ccm fs key)
