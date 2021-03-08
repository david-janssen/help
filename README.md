### What I am trying to do:

- I have an App that reads from the keyboard and writes to some simulated keyboard.
- I am trying to support Linux, OSX, and Windows.
- I am trying to support having a test-engine to run tests on configurations.

To that extent I wanted to write a polymorphic interface that allows some kind
of engine to be plugged into the app, where that engine takes care of reading
and writing keys.

The requirements for this engine:
- It needs to define a keycode type (different keycodes for different OSes, and
  a Char type for the test-engine.)
- It needs to provide monadic actions that read the next event, and write
  generated events to some output.
- It needs to be able to run a continuation, since we need to be able to run
  keyIO in a bracket-statement to allow for acquisition and cleanup.

### Currently broken

Isolated here is code isolated to compile on only RIO and Lens. It currently
fails with:

```
/home/david/prj/help/Confused.hs:42:15: error:
    • Couldn't match type ‘Keycode a’ with ‘Keycode a0’
      Expected type: (m (KeyEvent (Keycode a))
                      -> Const (m (KeyEvent (Keycode a))) (m (KeyEvent (Keycode a))))
                     -> a0 -> Const (m (KeyEvent (Keycode a))) a0
        Actual type: (m (KeyEvent (Keycode a0))
                      -> Const (m (KeyEvent (Keycode a))) (m (KeyEvent (Keycode a0))))
                     -> a0 -> Const (m (KeyEvent (Keycode a))) a0
      NB: ‘Keycode’ is a non-injective type family
      The type variable ‘a0’ is ambiguous
    • In the second argument of ‘(.)’, namely ‘getKeyL’
      In the second argument of ‘(^.)’, namely ‘keyIOL . getKeyL’
      In a stmt of a 'do' block: env ^. keyIOL . getKeyL
    • Relevant bindings include
        getKey :: m (KeyEvent (Keycode a)) (bound at Confused.hs:40:1)
   |
42 |   env^.keyIOL.getKeyL
   |               ^^^^^^^
```

I understand that Haskell doesn't realize that it should use the `Keycode a`
type from the `HasKeyIO` constraint in `getKey`, but I don't know how to explain
this to GHC.
