import System.Environment
import System.Console.HsOptions

main :: IO ()
main = getArgs >>= print . haqify . head

haqify s = "Required! " ++ s
