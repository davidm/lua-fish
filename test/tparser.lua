-- Test suite for luapegparser.lua
-- Note: this test suite if quite incomplete.

local LuaFishParser = require "luafish.parser"

local p = LuaFishParser()

-- return stringified AST of given code string.
local function sast(code)
  return tostring(p:parse(code))
end

local ops = {
  ['=='] = function(a,b) return a == b end,
  ['~='] = function(a,b) return a ~= b end
}
local function check(op, a, b)
  if not ops[op](a, b) then
    error(string.format("FAILED: %s\n  %s\n  %s", op, tostring(a), tostring(b)), 2)
  end
end

-- remove whitespace
local function nosp(s)
  return s:gsub("%s","")
end

-- load file into string
local function load_file(filename)
  local fh = assert(io.open(filename))
  local text = fh:read"*a"
  fh:close()
  return text
end


check('==', sast[[]], [[{tag="Block"}]])
check('==', sast[[return]], nosp[[{tag="Block",{tag="Return"} }]])
check('==', sast[[return 1]], nosp
  [[{tag="Block",{tag="Return",{tag="ExpList",{tag="Number",1} } } }]])
check('==', sast[[return a,b]], nosp
  [[{tag="Block",{tag="Return",{tag="ExpList",{tag="Id","a"},{tag="Id","b"} } } }]]) 
check('==', sast[[return 'a']], nosp
  [[{tag="Block",{tag="Return",{tag="ExpList",{tag="String","a"} } } }]])  -- removes quoting
check('==', sast[[return "a\"\""]], nosp
  [[{tag="Block",{tag="Return",{tag="ExpList",{tag="String","a\"\""} } } }]]) -- removes quoting
check('==', sast[=[return [[a]]]=], nosp
  [[{tag="Block",{tag="Return",{tag="ExpList",{tag="String","a"} } } }]])  -- removes quoting
check('==', sast[[return f(x)]], nosp
  [[{tag="Block",{tag="Return",{tag="ExpList",{tag="Call",{tag="Id","f"},
        {tag="ExpList",{tag="Id","x"} } } } } }]])
check('==', sast[[f(x)]], nosp
  [[{tag="Block",{tag="Call",{tag="Id","f"},{tag="ExpList",{tag="Id","x"} } } }]])
check('==', sast[[f.g(x)]], nosp[[
  {tag="Block",{tag="Call",{tag="Index",{tag="Id","f"},{tag="String","g"} },
        {tag="ExpList",{tag="Id","x"} } } }]])
check('==', sast[[f['g'](x)]], nosp[[
  {tag="Block",{tag="Call",{tag="Index",{tag="Id","f"},{tag="String","g"} },
        {tag="ExpList",{tag="Id","x"} } } }]])
check('==', sast[[f:g(x)]], nosp[[
  {tag="Block",{tag="ColonCall",{tag="Id","f"},{tag="String","g"},
        {tag="ExpList",{tag="Id","x"} } } }]])
check('==', sast[[f.g:h(x)]], nosp[[
  {tag="Block",{tag="ColonCall",
        {tag="Index",{tag="Id","f"},{tag="String","g"} },{tag="String","h"},
        {tag="ExpList",{tag="Id","x"} } } }]])
check('==', sast[[f(x)(y)]], nosp[[
 {tag="Block",
   {tag="Call",
     {tag="Call",{tag="Id","f"},{tag="ExpList",{tag="Id","x"} } },
     {tag="ExpList",{tag="Id","y"} } } }
]])
check('==', sast[[f.x(y)[z+v]:w{2}]], nosp[[
  {tag="Block",
    {tag="ColonCall",
      {tag="Index",
        {tag="Call",
          {tag="Index",{tag="Id","f"},{tag="String","x"} },
            {tag="ExpList",{tag="Id","y"} } },{tag="Op","+",{tag="Id","z"},{tag="Id","v"} } },
    {tag="String","w"},
    {tag="ExpList", {tag="Table",{tag="Field",{tag="Number",1},{tag="Number",2} } } } } }
]]) -- FIX-improve table AST

check('==', sast[[return a-b+c*d/e%f..g<h<=i>j>=k==l~=m and n or - not # p]], nosp[[
  {tag="Block", {tag="Return", {tag="ExpList",
    {tag="Op","Or",
      {tag="Op","And",
        {tag="Op","~=",
        {tag="Op","==",
        {tag="Op",">=",
        {tag="Op",">",
        {tag="Op","<=",
        {tag="Op","<",
          {tag="Op","..",
            {tag="Op","+",
              {tag="Op","-",
                {tag="Id","a"},
                {tag="Id","b"}
              },
              {tag="Op","%",
                {tag="Op","/",
                  {tag="Op","*",
                    {tag="Id","c"},
                    {tag="Id","d"}
                  },
                  {tag="Id","e"}
                },
                {tag="Id","f"}
              }
            },
            {tag="Id","g"}
          },
          {tag="Id","h"} },
          {tag="Id","i"} },
          {tag="Id","j"} },
          {tag="Id","k"} },
          {tag="Id","l"} },
          {tag="Id","m"}
        },
        {tag="Id","n"}
      },
      {tag="Op","Neg", {tag="Op","Not", {tag="Op","#", {tag="Id", "p"} } } }
    }
  } } }
]])

-- empty lists
check('==', sast[[f()]], nosp[[{tag="Block",{tag="Call",{tag="Id","f"},{tag="ExpList"}}}]])
check('==', sast[[function f() end]], nosp[[{tag="Block",{tag="FunctionDef",{tag="FuncName",{tag="Id","f"}},{tag="NameList"}, {tag="Block"}}}]])


--table
check('==', sast[[return {2,x=3,[y]=4}]], nosp[[
 {tag="Block",
   {tag="Return",
     {tag="ExpList",{tag="Table",
       {tag="Field",{tag="Number",1},{tag="Number",2} },
       {tag="Field",{tag="String","x"},{tag="Number",3} },
       {tag="Field",{tag="Id","y"},{tag="Number",4} }
 } } } }
]])


-- ensure it can parse iteself
assert(p:parse(load_file("lib/luafish/parser.lua")))

print 'done'

