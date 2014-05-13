module Main where

import Prelude
import Data.Foreign
import Math (floor)
import Control.Monad.Eff
import Control.Monad.Eff.Random
--import qualified Control.Monad.JQuery as J
--import qualified Graphics.Canvas as C
import Debug.Trace


-- used by canvas (I don't know it well)
foreign import data Context :: *

type Field = { height :: Number, width :: Number, cells :: [Number] }
type ScreenSize = { height :: Number, width :: Number }
type FPS = Number


foreign import onload
    "function onload(f) {\
    \  return function() {\
    \    window.onload = f;\
    \  };\
    \}" :: forall e a. Eff e a -> Eff e {}

foreign import after
    "function after(n) {\
    \  return function(f) {\
    \    window.setTimeout(f, n);\ 
    \  };\
    \}" :: forall e a. Number -> Eff e a -> Eff e {}

foreign import setGlobalVars
    "function setGlobalVars() { \
    \    window.SCREEN_SIZE = 500;  \
    \    window.SIDE_CELLS = 100;  \
    \    window.CELL_SIZE = SCREEN_SIZE / SIDE_CELLS;  \
    \}" :: forall e. Eff e {}

startLifegame :: forall e. FPS -> Field -> (Field -> Field) -> (Field -> Eff e {}) -> Eff e {}
startLifegame fps field update draw = loop field
  where
  loop field = do
    let next = update field
    draw next
    after (1000 / fps) (loop next)

foreign import update
    "function update(field) {\
    \    var cells = field.cells;\
    \    var n = 0;\
    \    var tempField = cells.slice();\
    \    for (var i=0; i<tempField.length; i++) {\
    \      n = 0;\
    \      for (var s=-1; s<2; s++) {\
    \        for (var t=-1; t<2; t++) {\
    \          if (s==0 && t==0) continue;\
    \          var c = i+s*SIDE_CELLS+t;\
    \          if (c>=0 && c<tempField.length) {\
    \            if (i<c && c%SIDE_CELLS!=0 || i>c && c%SIDE_CELLS!=SIDE_CELLS-1) {\
    \              if (tempField[c]) n ++;\
    \            }\
    \          }\
    \        }\
    \      }\
    \      if (tempField[i] && (n==2||n==3)) {\
    \        cells[i] = 1;\
    \      } else if (!tempField[i] && n==3) {\
    \        cells[i] = 1;\
    \      } else cells[i] = 0;\
    \    }\
    \    field.cells = cells;\
    \    return field;\
    \}" :: Field -> Field

foreign import draw
    "function draw(context) {\
    \  return function(field) {\
    \    return function() {\
    \      var cells = field.cells;\
    \      context.clearRect(0, 0, SCREEN_SIZE, SCREEN_SIZE);\
    \      for (var i=0; i<cells.length; i++) {\
    \        var x = (i%SIDE_CELLS)*CELL_SIZE;\
    \        var y = Math.floor((i/SIDE_CELLS))*CELL_SIZE;\
    \        if (cells[i]) context.fillRect(x, y, CELL_SIZE, CELL_SIZE);\
    \      }\
    \    };\
    \  };\
    \}" :: forall e. Context -> Field -> Eff e {}

-- depends on html.
foreign import getContext
    "function getContext(elementId) {\
    \  return function(screenSize) {\
    \    return function() {\
    \      var canvas = document.getElementById(elementId);  \
    \      canvas.width = canvas.height = SCREEN_SIZE;  \
    \      var scaleRate = Math.min(screenSize.height/SCREEN_SIZE, screenSize.width/SCREEN_SIZE);  \
    \      canvas.style.width = canvas.style.height = SCREEN_SIZE*scaleRate+'px';  \
    \      var context = canvas.getContext('2d');  \
    \      context.fillStyle = 'rgb(211, 85, 149)';  \
    \      return context; \
    \    }; \
    \  }; \
    \}" :: forall e. String -> ScreenSize -> Eff e Context

foreign import getScreenSize
    "function getScreenSize() {\
    \  return { height: window.innerHeight, width: window.innerWidth };\
    \}" :: forall e. Eff e ScreenSize
    
foreign import makeRandomArray
    "function makeRandomArray(initializer) {\
    \  return function(n) {\
    \    return function() {\
    \        var field = new Array(n);  \
    \        for (var i=0; i<field.length; i++) field[i] = initializer();  \
    \        return field;\
    \    };\
    \  };\
    \}" :: forall r. Eff (random :: Random | r) Number -> Number -> Eff (random :: Random | r) [Number]

createField :: forall r. Number -> Number -> Eff (random :: Random | r) Field
createField h w = do
    cells <- makeRandomArray initializer (h * w)
    return { height: h, width: w, cells: cells }
  where
    initializer = do
        r <- random
        return $ floor (r * 2)



main = onload $ do
    let fps = 10
    let cellNum = 100
    setGlobalVars
    screen <- getScreenSize
    ctx <- getContext "world" screen
    initField <- createField cellNum cellNum
    startLifegame fps initField update (draw ctx)
