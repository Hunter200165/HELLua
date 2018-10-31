--[[
    JITed version of HELLua-Builder
    Version of Lua Virtual Machine: Lua 5.1
    Copyright (c) Hunter200165, 2018
    All rights reserved
]]

local LuaJITName = 'luajit.exe';
local BuilderName = 'HELLua_Builder.lua';

local Args = {...};

for i = 1, #Args do 
    Args[i] = ('%q'):format(Args[i]);
end;

do 
    os.execute(('%s %s '):format(LuaJITName, BuilderName) .. table.concat(Args));
end;