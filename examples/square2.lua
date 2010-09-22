-- square2.lua
-- LuaFish example of a module that indirectly
-- contains macros.  Contains both
-- static type check and run-time behavior.

-- Static type definition.
local TSquare = {}; do
  print 'DEBUG:square2:begin compiletime'  -- trace

  local Macro = require "luafish.macro"

  -- Helper functions.
  local report = function(...) print('ERROR:', ...) end
  local check = function(test,message)
  if not test then report(message) else return true end
  end
  
  setmetatable(TSquare, {
    __tostring = function() return '[TSquare Class]' end
  })
  -- bind lexical to this type.
  function TSquare.bind(obj_ast)
    obj_ast.stype = TSquare
  end
  -- tests if expression is of this type
  function TSquare.isa(obj_ast)
    return 'value', obj_ast.stype == TSquare
  end
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
        check(Macro.TString.isa(o.stype), 'second param must be string')
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
  function TSquare:__mul(other)
    print('static __mul', self, other)
    if not (check(stype == TSquare, 'first op must be TSquare') or
            check(Macro.TNumber.isa(other), 'second op must be number'))
    then return end
    return TSquare
  end
  print 'DEBUG:square2:end compiletime'
end

-- Run-time behavior.
TSquare.class = require "luafish.staticmodule" {} [[
  print 'DEBUG:square2:begin runtime'

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
  
  print 'DEBUG:square2:end runtime'

  return Square
]]

return TSquare

