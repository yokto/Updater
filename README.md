# Updater
Monadic FRP library

*Warning Updater is not stable yet*

[API on Hackage](http://hackage.haskell.org/package/Updater)

This is supposed to be a practical (simple to use) approach to Reactive programming in Haskell.

In the implementation there is only one Object called Updater.
For

    updater1 >>= \result -> updater2 result

updater1 can call updater2 whenever it want with some event value.
In the API, we distinguish Events (push based) and Behaviors (pull based).
A Behavior is guaranteed to call the next Behavior only once.

# Getting started

If you set the following you will be able to copy paste the example(s) to ghci.

    :set +m
    :m +Updater
    :m +Control.Applicative

So let's make some events.

    (eventA, buttonA) <- newEvent :: IO (Event Int, Int -> IO ())
    (eventB, buttonB) <- newEvent :: IO (Event Int, Int -> IO ())

Now let's see what we can do with them.

    runGlobalEvent $ do
        (a,b) <- (,) <$> eventA <*> eventB
        if a+b == 42
            then return $ putStrLn "Hurray 42"
            else return $ putStrLn $ "sum: " ++ show (a+b)

Now you can run Events.

    buttonA 1
    buttonB 2
    buttonA 40

For a more complicated example you can have a look at the Example folder.

# Semantics

TODO