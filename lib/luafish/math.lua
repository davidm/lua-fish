local Macro = require "luafish.macro"
local TNumber = Macro.TNumber

-- static type for "math" object.
local tmath = {}

-- check range of arg a against param p
local function check_range(a, p)
  if type(p) == 'table' and a.value then
    local min, max = p[2], p[3]
    if min == 0 and a.value < min then
      return false, 'must be non-negative'
    elseif min and a.value < min then
      return false, 'must be no less than ' .. min
    end
    if max == 0 and a.value > max then
      return false, 'must be non-positive'
    elseif max and a.value > max then
      return false, 'must be no greater than ' .. max
    end
   end
  return true
end

-- check range of arg a against param p
local function checkvar(a, p)
  if a == nil then return true end -- unknown
  if p == 'N' or type(p) == 'table' and p[1] == 'N' then
    if not TNumber.isa(a) then
      return false, 'must be a number'
    end
    local status, message = check_range(a, p)
    if not status then return status, message end 
  elseif p == 'I' or type(p) == 'table' and p[1] == 'I' then
    if not TNumber.isa(a) then
      return false, 'must be an integer'
    end
    local status, message = check_range(a, p)
    if not status then return status, message end
  else
    assert(false)
  end
  return true
end

local function functioncheck(...)
  local name = ...
  local t = {select(2, ...)}  -- arg types

  -- improve?
  local obj = assert(loadstring('return ' .. name))()

  local nparams = 0
  for i=1,#t do
    if t[i] == '->' then break end
    nparams = nparams + 1
  end

  return function(...)
    local n = select('#', ...)
    assert(n == nparams, name .. ' expects ' .. nparams .. ' argument(s)')
    local is_return = false
    for i=1,n do
      local a_ast = select(i, ...)
      local p = t[i]
      local status,message = checkvar(a_ast.stype,p)
      if not status then error(name .. ' arg ' .. i .. ' ' .. message) end
      -- FIX:improve to make more specific, including ranges
    end

    if nparams < #t then
      assert(nparams+1+1 == #t, 'NOT IMPL')
      --FIX: handle multiple returns?
      --print(obj, ...)
      local args = {}
      local is_defined = true
      for i=1,n do
        local a_ast = select(i, ...)
        args[i] = a_ast.stype and a_ast.stype.value
        if args[i] == nil then is_defined = false; break end
      end
      if is_defined then      
        return TNumber.create(obj(unpack(args))) -- fix:assumes no side effect like randomseed
      end
    end
  end
end

local funcs = {}

--FIX check all these
funcs.abs    = functioncheck('math.abs', 'N', '->', {'N',0,math.huge})
funcs.acos   = functioncheck('math.acos', {'N',-1,1}, '->', {'N',0,math.pi/2})
funcs.asin   = functioncheck('math.asin', {'N',-1,1}, '->', {'N',-math.pi/2,math.pi/2})
funcs.atan   = functioncheck('math.atan', {'N',-math.huge,math.huge}, '->',
                         {'N',-math.pi/2,math.pi/2})
--FIX funcs.atan2
funcs.ceil   = functioncheck('math.ceil', 'N','->','I')
funcs.cos    = functioncheck('math.cos', 'N','->',{'N',-1,1})
funcs.cosh   = functioncheck('math.cosh', 'N','->',{'N',1,math.huge})
funcs.deg    = functioncheck('math.deg', 'N','->','N')
funcs.exp    = functioncheck('math.exp', 'N','->',{'N',0,math.huge})
funcs.floor  = functioncheck('math.floor', 'N','->','I')
funcs.fmod   = functioncheck('math.fmod', 'N','N','->','N')
funcs.frexp  = functioncheck('math.frexp', 'N','->',{'N',-1,1},'->','I')
funcs.huge   = TNumber.create(math.huge)
funcs.ldexp  = functioncheck('math.ldexp', {'N','I'},'->','N')
funcs.log    = functioncheck('math.log', {'N',0,math.huge},'->','N')
funcs.log10  = functioncheck('math.log10', {'N',0,math.huge},'->','N')
function max(...)
  print 'NOT IMPL'
end
function min(...)
  print 'NOT IMPL'
end
funcs.modf = functioncheck('math.modf', 'N','->','I',{'N',-1,1})
funcs.pi   = TNumber.create(math.pi)
funcs.pow  = functioncheck('math.pow', 'N','N','->','N') -- improve?
funcs.rad  = functioncheck('math.rad', 'N','->','N')
funcs.random = function()
  print 'NOT IMPL'
end
funcs.randomseed = functioncheck('math.randomseed', 'N')
funcs.sin  = functioncheck('math.sin', 'N','->',{'N',-1,1})
funcs.sinh = functioncheck('math.sinh', 'N','->','N')
funcs.sqrt = functioncheck('math.sqrt', {'N',0,math.huge},'->',math.huge)
funcs.tan  = functioncheck('math.tan', 'N','->','N') -- improve?
funcs.tanh = functioncheck('math.tanh', 'N','->',{'N',-1,1})


function tmath.__index(t, k)
  assert(math[k], 'key ' .. tostring(k) .. ' not defined in math')

  return funcs[k] 
end

return tmath
