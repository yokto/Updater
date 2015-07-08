# Updater
Monadic FRP library based on stm

*Warning Updater is not stable yet*

[API on Hackage](http://hackage.haskell.org/package/Updater)

This is supposed to be a practical (simple to use) approach to Reactive programming in Haskell.
We basically have the portable version of signals.

    data State = ...

This is basically just an TVar from stm with some update notifiers.
It is completely portable and can be used anywhere in your program.

If you set the following you will be able to copy paste the examples to ghci.

    :set +m
    :m +Control.Concurrent
    :l +Updater

So let's make a signal.

    (button, signal) <- runUpdater newSignal :: IO (Int -> Updater (), Signal Int)
    runUpdater (getValue signal) >>= print -- Nothing
    runUpdater $ button 42
    runUpdater (getValue signal) >>= print -- Just 42
    

This is the way of using signals manually.
However, it is much more confortable to use the Updater monad to combine the signals.
Let's have a look at an example.

    (buttonA, signalA) <- runUpdater newSignal :: IO (Int -> Updater (), Signal Int)
    (buttonB, signalB) <- runUpdater newSignal :: IO (Int -> Updater (), Signal Int)
    
    forkIO $ runUpdater $ do
        a <- getBehavior signalA
        b <- getBehavior signalB
        putLine $ "sum " ++ show (a+b)
        if a+b == 42
            then putLine "Hurray 42. We're done now"
            else stop
    
Now you can use the following to set the signals.

    runUpdater $ buttonA 1
    runUpdater $ buttonB 2

As soon as they add up do 42 the forked Updater will return and unregister everything.
So how does it work?
`getBehavior` will basically check the signal.
If the signal is already initialize it will continue executing otherwise it will wait.
Then everytime it the signal is fired it will reececute everything down from it.
`getEvent` does the same except without the initial run.

For a more complicated example you can have a look at the Example folder.

    