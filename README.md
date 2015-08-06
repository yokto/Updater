# Updater
Monadic FRP library based on stm

*Warning Updater is not stable yet*

[API on Hackage](http://hackage.haskell.org/package/Updater)

This is supposed to be a practical (simple to use) approach to Reactive programming in Haskell.
We basically have the portable version of signals.

    data Signal value = ...

This is basically just an TVar from stm with some update notifiers.
It is completely portable and can be used anywhere in your program.

If you set the following you will be able to copy paste the examples to ghci.

    :set +m
    :m +Control.Concurrent
    :m +Updater

So let's make a signal.

    signal <- runUpdater $ newSignal 0
    runUpdater (readSignal signal) -- 0
    runUpdater (writeSignal signal 42)
    runUpdater (readSignal signal) -- 42
    

This is the way of using signals manually.
However, it is much more comfortable to use the Updater monad to combine the signals.
Let's have a look at an example.

    signalA <- runUpdater $ newSignal 0
    signalB <- runUpdater $ newSignal 0
    
    forkIO $ runUpdater $ do
        a <- getBehavior signalA
        b <- getBehavior signalB
        putLine $ "sum " ++ show (a+b)
        if a+b == 42
            then putLine "Hurray 42. We're done now"
            else stop

The stop keyword doesn't mean that we stop the Updater but that we don't run any further.
When Updater is run through to the end with no stop it will return that value.
Now you can use the following to write the signals.

    runUpdater $ writeSignal signalA 1
    runUpdater $ writeSignal signalB 2

As soon as they add up do 42 the forked Updater will return and unregister everything.
So how does it work?
`getBehavior` will basically check the signal.
If the signal is already initialize it will continue executing otherwise it will wait.
Then everytime it the signal is fired it will reececute everything down from it.
`getEvent` does the same except without the initial run.

For a more complicated example you can have a look at the Example folder.

# Semantics

Of course I don't have a complete semantic of the framework but this should give you an idea about what assurances you have.

1) The only time any stm transaction (signal update) takes place is right at the start of a runUpdater call.
   Any later updates, that appear to happen in a runUpdater call, usually happen because runUpdater is also
   executed in an other thread that is waiting for an external signal. Such as TODO.

2) When executing updating a signal using it's button, the value is changed immediately and can be querried using
   getValue. However any variables previously aquiered by getEvent will still be at their old value.
   As soon as all listeners to the current value are finnished running, they will run with the new value.
   In the following example this should ensure that the action is executed  all values of x and y exactly once.
       do
           x <- getEvent signalX
           y <- getEvent signalY
           action x y

3) Any io actions scheduled via onCommit will run as soon as all stm actions in a runUpdater have been commited.
   They are run in the order in which they are given in the Updater monad or parallel if in the case of Alternative for instance.
   runUpdater will only return once all io actions have completed.

4) todo exception handling