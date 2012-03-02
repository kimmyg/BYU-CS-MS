

we were passed the stack of continuation marks
we set it in the frame and pass it along...?

[Frame]

wcm f:fs key value g = g ((frameSet f key value):fs)

factorial 0 frames = (1, frames)
factorial n f:fs = 

= n * factorial []:frames (n - 1)

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
