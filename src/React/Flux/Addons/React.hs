-- | Bindings for the <https://facebook.github.io/react/docs/addons.html React addons> that make
-- sense to use from Haskell.  At the moment, that is only the
-- <https://facebook.github.io/react/docs/animation.html animation> and
-- <https://facebook.github.io/react/docs/perf.html performance tools>.
module React.Flux.Addons.React (
  -- * Animation
    cssTransitionGroup

  -- * Perf
  , PerfAction(..)
  , PerfPrint(..)
  , perfToggleButton_
  , perfA
) where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import GHC.Generics (Generic)
import React.Flux

#ifdef __GHCJS__
import GHCJS.Types (JSVal, JSString)
#endif

-- | The <https://facebook.github.io/react/docs/animation.html ReactCSSTransitionGroup> element.
-- For example in React 0.14,
--
-- >cssTransitionGroup ["transitionName" $= "example", transitionAppear @= True, transitionAppearTimeout @= (100 :: Int)] $
-- >    h1_ "Fading at initial mount"
cssTransitionGroup :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler a -> ReactElementM eventHandler a

#ifdef __GHCJS__
cssTransitionGroup props children = foreignClass js_CSSTransitionGroup props children

foreign import javascript unsafe
    "React['addons']['CSSTransitionGroup']"
    js_CSSTransitionGroup :: JSVal

#else
cssTransitionGroup _ x = x
#endif

--------------------------------------------------------------------------------
-- Perf
--------------------------------------------------------------------------------

data PerfStoreData = PerfStoreData { perfIsActive :: Bool }

-- | What to print after stopping performance measurement.  See
-- <https://facebook.github.io/react/docs/perf.html> for documentation.
data PerfPrint = PerfPrintInclusive
               | PerfPrintExclusive
               | PerfPrintWasted
               | PerfPrintDOM
    deriving (Show, Eq, Generic)

instance NFData PerfPrint

-- | An action to start or stop performance measurement.  For details, see
-- <https://facebook.github.io/react/docs/perf.html>.
data PerfAction = PerfStart
                | PerfStopAndPrint [PerfPrint]
    deriving (Show, Eq, Generic)

instance NFData PerfAction

transformPerfStoreData
  :: PerfAction
  -> PerfStoreData
  -> IO PerfStoreData
transformPerfStoreData PerfStart _ = do
    js_perf "start"
    return $ PerfStoreData True
transformPerfStoreData (PerfStopAndPrint toPrint) _ = do
    js_perf "stop"
    forM_ toPrint $ \action -> do
        js_perf $ case action of
            PerfPrintInclusive -> "printInclusive"
            PerfPrintExclusive -> "printExclusive"
            PerfPrintWasted -> "printWasted"
            PerfPrintDOM -> "printDOM"
    return $ PerfStoreData False

perfStore :: ReactStore PerfStoreData
perfStore = mkStore $ PerfStoreData False

-- | Convert a performance action into a store action.   Use this if you are not using
-- 'perfToggleButton_'.
perfA :: PerfAction -> SomeStoreAction
perfA = mkSomeStoreAction transformPerfStoreData perfStore

-- | The performance toggle button view
perfToggleButton :: ReactView [PerfPrint]
perfToggleButton =
  defineControllerView "perf toggle button" perfStore
  $ \sData toPrint ->
    button_ [ onClick $ \_ _ ->
                if perfIsActive sData
                    then [perfA $ PerfStopAndPrint toPrint]
                    else [perfA PerfStart]
            ] $
        if perfIsActive sData then "Stop perf measurement" else "Start perf measurement"

-- | A button which when clicked toggles the performance measurement on and off.  When the
-- measurement is stopped, the given measurements are printed.  If you want more control over the
-- performance tools, you can use 'perfA' directly from your own event handlers.
perfToggleButton_ :: [PerfPrint] -> ReactElementM handler ()
perfToggleButton_ toPrint = view perfToggleButton toPrint mempty

#ifdef __GHCJS__

foreign import javascript unsafe
    "React['addons']['Perf'][$1]()"
    js_perf :: JSString -> IO ()

#else

js_perf :: String -> IO ()
js_perf _ = return ()

#endif
