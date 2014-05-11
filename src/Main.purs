module Main where

import Prelude
import Data.Foreign
import Control.Monad.Eff
import Control.Monad.Eff.Random
--import qualified Control.Monad.JQuery as J
--import qualified Graphics.Canvas as C
--import Debug.Trace


-- used by canvas (I don't know it well)
foreign import data Context :: *

type Field = [Number]
type FPS = Number

foreign import onload
    "function onload(f) {\
    \  return function() {\
    \    window.onload = f;\
    \  };\
    \}" :: forall e a. Eff e a -> Eff e {}

foreign import setGlobalVars
    "function setGlobalVars() { \
    \    window.SCREEN_SIZE = 500;  \
    \    window.SIDE_CELLS = 100;  \
    \    window.CELL_SIZE = SCREEN_SIZE / SIDE_CELLS;  \
    \}" :: forall e a. Eff e {}

foreign import startLifegame
    "function startLifegame(fps) {\
    \  return function(field) {\
    \    return function(update) {\
    \      return function(draw) {\
    \        return function() {\
    \            var next = update(field)();\
    \            draw(next)();\
    \            setTimeout(startLifegame(fps)(next)(update)(draw), 1000/fps);\
    \        };\
    \      };\
    \    };\
    \  };\
    \}" :: forall e. FPS -> Field -> (Field -> Field) -> (Field -> Eff e {}) -> Eff e {}

foreign import update
    "function update(field) {\
    \  return function() {\
    \    var n = 0;  \
    \    var tempField = field.slice();  \
    \    for (var i=0; i<tempField.length; i++) { \
    \      n = 0; \
    \      for (var s=-1; s<2; s++) { \
    \        for (var t=-1; t<2; t++) { \
    \          if (s==0 && t==0) continue;  \
    \          var c = i+s*SIDE_CELLS+t;  \
    \          if (c>=0 && c<tempField.length) {  \
    \            if (i<c && c%SIDE_CELLS!=0 || i>c && c%SIDE_CELLS!=SIDE_CELLS-1) {  \
    \              if (tempField[c]) n ++;  \
    \            } \
    \          } \
    \        } \
    \      } \
    \      if (tempField[i] && (n==2||n==3)) {  \
    \        field[i] = 1;  \
    \      } else if (!tempField[i] && n==3) {  \
    \        field[i] = 1;  \
    \      } else field[i] = 0;  \
    \    } \
    \    return field;\
    \  }; \
    \}" :: Field -> Field

foreign import draw
    "function draw(context) { \
    \  return function(field) { \
    \    return function() {\
    \      context.clearRect(0, 0, SCREEN_SIZE, SCREEN_SIZE); \
    \      for (var i=0; i<field.length; i++) { \
    \        var x = (i%SIDE_CELLS)*CELL_SIZE; \
    \        var y = Math.floor((i/SIDE_CELLS))*CELL_SIZE; \
    \        if (field[i]) context.fillRect(x, y, CELL_SIZE, CELL_SIZE); \
    \      } \
    \    }; \
    \  }; \
    \}" :: forall e. Context -> Field -> Eff e {}

-- Dependent html.
foreign import getContext
    "function getContext() {\
    \  var canvas = document.getElementById('world');  \
    \  canvas.width = canvas.height = SCREEN_SIZE;  \
    \  var scaleRate = Math.min(window.innerHeight/SCREEN_SIZE, window.innerHeight/SCREEN_SIZE);  \
    \  canvas.style.width = canvas.style.height = SCREEN_SIZE*scaleRate+'px';  \
    \  var context = canvas.getContext('2d');  \
    \  context.fillStyle = 'rgb(211, 85, 149)';  \
    \  return context; \
    \}" :: forall e. Eff e Context
    
foreign import createField
    "function createField(h) {\
    \  return function(w) {\
    \    return function() {\
    \      var field = new Array(h * w);  \
    \      for (var i=0; i<field.length; i++) field[i] = Math.floor(Math.random()*2);  \
    \      return field;\
    \    };\
    \  };\
    \}" :: forall r. Number -> Number -> Eff (random :: Random | r) Field


main = onload $ do
    let fps = 2
    let cellNum = 100
    setGlobalVars
    ctx <- getContext
    initField <- createField cellNum cellNum
    draw ctx initField
    startLifegame fps initField update (draw ctx)
