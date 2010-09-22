-- square.lua
-- Lua fish example module with macros.

-- This function inside the ONCOMPILE macro is executed
-- at compile-time.  This defines the static type-checking code.
ONCOMPILE(function()
  print 'square:begin compiletime'  -- trace

  -- Helper functions.
  local report = function(...) print('ERROR:', ...) end
  local check = function(test,message)
    if not test then report(message) end
  end

  -- Define static type TSquare.
  local TSquare = {}
  setmetatable(TSquare,
      {__tostring = function() return '[TSquare Class]' end})
  local is_method = {area=true,perimeter=true,setcolor=true}
  function TSquare:__index(k)
    print('static __index', self, k)
    if not is_method[k] then
      report(tostring(k) .. ' not in ' .. tostring(TSquare))
    end
    if k == 'setcolor' then
      return function(self, o, ...)
        print('static call', self, o, ...)
        check(self.stype == TSquare, 'first param must be TSquare')
        check(M.TString.isa(o.stype), 'second param must be string')
        if select('#', ...) ~= 0 then
          report('expected two arguments')
        end
      end
    else
      return function(self, ...)
        print('static call', self, ...)
        if select('#', ...) ~= 0 then
          report('expected zero arguments')
        end
      end      
    end
  end

  -- Define compile-time macro to retrieve TSquare static type.
  --function MACRO.GETSQUARE() return 'value', TSquare end
  _G.TSquare = TSquare -- hack

  print 'square:end compiletime'
end)


print 'square:begin runtime'

-- Define run-time class.
local Square = {}
Square.__index = Square
function Square.create(length)
  return setmetatable({length=length}, Square)
end
function Square:area(length) return self.length^2 end
function Square:perimeter(length) return self.length*4 end
function Square:setcolor(color) self.color = color end
function Square:__mul(other, val)
  return Square.create(self.length * val)
end

--not usable right now: Square.TYPE = GETSQUARE()

print 'square:end runtime'

return Square
