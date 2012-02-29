-- CMM - Continuation Mark Monad

type Key = String
type Value = String
type CMM a = (a, [(Key,Value)])

cmmReturn :: a -> CMM a
cmmReturn x = (x, [])

cmmBind :: CMM a -> (a -> CMM b) -> CMM b
cmmBind (x, cms) f = let (y, new_cms) = f x in (y, new_cms++cms)

wcm :: CMM a -> Key -> Value -> (CMM a -> CMM b) -> CMM b
wcm (x, cms) key value f = f (x, ((key, value):cms))

ccm :: CMM a -> Key -> [Value]
ccm (_, cms) key = [ value | (key', value) <- cms, key' == key ]

--instance Monad (CMM a) where
--return x = cmmReturn x
--x >>= f = cmmBind x f

main = print (cmmReturn 5)
