module System.Console.HsOptions.Parser
where

type ParsedFlag = (String, String)
type ParseStream = [String]

parseFlag :: ParseStream -> (ParsedFlag, ParseStream)
parseFlag [] = undefined
parseFlag [x] = ((x, ""),[])
parseFlag (x1:x2:xs) = (flag, xs)
    where flag = (drop 2 x1, x2)

