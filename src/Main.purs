module Main where

--import Data.Either
import Prelude
import Data.Foreign
import Control.Monad.Eff
--import qualified Control.Monad.JQuery as J
--import qualified Graphics.Canvas as C
--import Debug.Trace


foreign import onLoad
    "function onLoad(f) {\
    \  return function() {\
    \    window.onload = f;\
    \  };\
    \}" :: forall e a. Eff e a -> Eff e {}

foreign import setGlobalVars
    "function setGlobalVars() { \
    \    window.SCREEN_SIZE = 500;  \
    \    window.SIDE_CELLS = 100;  \
    \    window.CELL_SIZE = SCREEN_SIZE / SIDE_CELLS;  \
    \    window.FPS = 2;  \
    \        var field = new Array(SIDE_CELLS*SIDE_CELLS);  \
    \        var tempField = new Array(SIDE_CELLS*SIDE_CELLS);  \
    \        for (var i=0; i<field.length; i++) field[i] = Math.floor(Math.random()*2);  \
    \        var canvas = document.getElementById('world');  \
    \        canvas.width = canvas.height = SCREEN_SIZE;  \
    \        var scaleRate = Math.min(window.innerHeight/SCREEN_SIZE, window.innerHeight/SCREEN_SIZE);  \
    \        canvas.style.width = canvas.style.height = SCREEN_SIZE*scaleRate+'px';  \
    \        var context = canvas.getContext('2d');  \
    \        context.fillStyle = 'rgb(211, 85, 149)';  \
    \    window.field = field;\
    \    window.tempField = tempField;\
    \    window.context = context;\
    \}" :: forall e a. Eff e {}

foreign import setIntervalLG
    "function setIntervalLG() {\
    \  setInterval(window.update, 1000/window.FPS, window.field, window.tempField, window.context);\
    \};" :: forall e. Eff e {}

--    \};" :: forall a b c e. (a -> b -> c -> Eff e {}) -> Eff e {}

foreign import setUpdate
    "function setUpdate() { \
    \    var draw = function (field, context) { \
    \        context.clearRect(0, 0, SCREEN_SIZE, SCREEN_SIZE);  \
    \        for (var i=0; i<field.length; i++) { \
    \            var x = (i%SIDE_CELLS)*CELL_SIZE;  \
    \            var y = Math.floor((i/SIDE_CELLS))*CELL_SIZE;  \
    \            if (field[i]) context.fillRect(x, y, CELL_SIZE, CELL_SIZE);  \
    \        }  \
    \    }; \
    \    var _update = function(field, tempField, context) {\
    \        var n = 0;  \
    \        tempField = field.slice();  \
    \        for (var i=0; i<tempField.length; i++) { \
    \            n = 0; \
    \            for (var s=-1; s<2; s++) { \
    \                for (var t=-1; t<2; t++) { \
    \                    if (s==0 && t==0) continue;  \
    \                    var c = i+s*SIDE_CELLS+t;  \
    \                    if (c>=0 && c<tempField.length) {  \
    \                        if (i<c && c%SIDE_CELLS!=0 || i>c && c%SIDE_CELLS!=SIDE_CELLS-1) {  \
    \                            if (tempField[c]) n ++;  \
    \                        } \
    \                    } \
    \                } \
    \            } \
    \            if (tempField[i] && (n==2||n==3)) {  \
    \                field[i] = 1;  \
    \            } else if (!tempField[i] && n==3) {  \
    \                field[i] = 1;  \
    \            } else field[i] = 0;  \
    \        } \
    \        draw(field, context);  \
    \    }; \
    \ \
    \  window.update = _update;\
    \}" :: forall e. Eff e {}

    

main = onLoad $ do
    setGlobalVars
    setUpdate
    setIntervalLG
