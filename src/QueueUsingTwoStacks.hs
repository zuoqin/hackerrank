module QueueUsingTwoStacks
    ( queueUsingTwoStacks, Stack, empty, isEmpty, push, top, pop
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


builddestack :: (Stack Int, Stack Int) -> (Stack Int, Stack Int)
builddestack (stack1,stack2)
  | isEmpty stack1 = (stack1, stack2)
  | otherwise = do
    let x = top stack1
    builddestack( (snd (pop stack1)), push x stack2)

  
buildenstack :: (Stack Int, Stack Int) -> (Stack Int, Stack Int)
buildenstack (stack1,stack2)
  | isEmpty stack2 = (stack1, stack2)
  | otherwise = buildenstack( (push (top stack2) stack1),(snd (pop stack2)))

enqueue :: Int -> (Int, Stack Int, Stack Int) -> (Int, Stack Int, Stack Int)
enqueue x (front, stack1, stack2) = do
  let (newstack1, newstack2) = buildenstack(stack1,stack2)
  if( isEmpty newstack1 )
  then (x, (push x newstack1), newstack2)
  else (front, (push x newstack1), newstack2)

dequeue :: (Int, Stack Int, Stack Int) -> (Int, Stack Int, Stack Int)
dequeue (front, stack1, stack2) = do
  let (newstack1, newstack2) = builddestack(stack1,stack2)
  let newstack3 = snd (pop newstack2)
  if( isEmpty newstack3 )
  then (front, newstack1, newstack3)
  else ((top newstack3), newstack1, newstack3)

processop :: Int -> Int -> (Int, Stack Int, Stack Int) -> (Int, Stack Int, Stack Int)
processop a b (front, stack1, stack2)
  | a == 1 = enqueue b (front, stack1, stack2)
  | a == 2 = dequeue (front, stack1, stack2)
  | otherwise = (front, stack1, stack2)

queueUsingTwoStacksCases :: Int -> (Int, Stack Int, Stack Int) -> IO()
queueUsingTwoStacksCases cases (front, stack1, stack2)
    | cases < 1 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let a = read $ x_t!!0 :: Int
        let b = if( length x_t ) > 1 then read $ x_t!!1 :: Int else 0

        if a == 3
        then do 
            print front
            queueUsingTwoStacksCases (cases-1) (front, stack1, stack2  )
        else queueUsingTwoStacksCases (cases-1) (processop a b (front, stack1, stack2  ))

queueUsingTwoStacks ::  IO ()
queueUsingTwoStacks = do
    n_temp <- getLine
    let n = read n_temp :: Int
    queueUsingTwoStacksCases n (0, empty, empty)
    
