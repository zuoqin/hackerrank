module BalancedBrackets
    ( balancedBrackets
    ) where


empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)
 
newtype Stack a = StackImpl [a] -- opaque!
empty = StackImpl []
isEmpty (StackImpl s) = null s
push x (StackImpl s) = StackImpl (x:s)
top (StackImpl s) = head s
pop (StackImpl (s:ss)) = (s,StackImpl ss)


balancedBracketsCases :: Int -> IO()
balancedBracketsCases cases
    | cases < 1 = putStr ""
    | otherwise = do
        x_temp <- getLine

        let res = foldl checkCase (empty, True) x_temp
                  where checkCase (stack, res) ch =
                           if res == False
                           then (empty, False)
                           else    
                               if ch == '(' || ch == '[' || ch == '{'
                               then (push ch stack, True)
                               else if (isEmpty stack) == True
                                    then(empty, False) 
                                    else do
                                           let newstack = pop stack
                                           let top1 = fst newstack
                                           if (ch == ')' && top1 == '(') || (ch == ']' && top1 == '[') || (ch == '}' && top1 == '{')
                                           then ( (snd newstack), True )
                                           else (empty, False)
        if (snd res) == False
        then putStrLn "NO"
        else if isEmpty (fst res)
             then putStrLn "YES"
             else putStrLn "NO"
        balancedBracketsCases (cases-1)

balancedBrackets ::  IO ()
balancedBrackets = do
    n_temp <- getLine
    let n = read n_temp :: Int
    balancedBracketsCases n
    
