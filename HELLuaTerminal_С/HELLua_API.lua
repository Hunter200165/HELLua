 -- HELLua API for basic features.
-- Copyright (c) Hunter200165, 2018.
-- MIT Licensed

-- Basic API for Class API

local HELLua_API = {};

-- Implementation


local GetEnvironment = getfenv;
local SetEnvironment = setfenv;
local SetMetaTable = setmetatable;
local GetMetaTable = getmetatable;

local CurrentStackLevel = 1;
local PreviousStackLevel = 2;

local type = type;
local tostring = tostring;

function _G.with(Object, doFunction)
  	local Environment = {};
  	local OldEnv = GetEnvironment(PreviousStackLevel);
  	local EnvMeta = {};

  	function EnvMeta.__index(self, index)
    	local PreResult = Object[index];
    	if PreResult == nil then
      		local Level = 2;
      		while (not (debug.getinfo(Level) == nil)) do
        		local CountLevel = 1;
        		local Name, Value = debug.getlocal(Level, CountLevel);
        		while not (Name == nil) do
          			-- print(Level, CountLevel);
          			-- print('Name: ', Name, '\tValue: ', Value);
        			if Name == index then return Value; end;
        			CountLevel = CountLevel + 1;
        			Name, Value = debug.getlocal(Level, CountLevel);
        		end;
        		Level = Level + 1;
      		end;
      		return OldEnv[index];
    	else
      		return PreResult;
    	end;
  	end;

  	function EnvMeta.__newindex(self, index, value)
		-- rawset(self, index, value);
		Object[index] = value;
  	end;

  	Environment.__ENV = OldEnv;
  	Environment.self = Object;
  	Environment.this = Object;

  	Environment = SetMetaTable(Environment, EnvMeta);

  	return SetEnvironment(doFunction, Environment)();
end;

function _G.strictly_with(Object, doFunction)
	local Environment = {};
	local OldEnv = GetEnvironment(PreviousStackLevel) or {};
	local EnvMeta = {};

	function EnvMeta.__index(self, index)
		local PreResult = Object[index];
		return PreResult;
	end;

	function EnvMeta.__newindex(self, index, value)
		Object[index] = value;
	end;

	Environment.__ENV = OldEnv;
	Environment.self = Object;
	Environment.this = Object;

	Environment = SetMetaTable(Environment, EnvMeta);

	return SetEnvironment(doFunction, Environment)();
end;

function Try(TryF, ExceptF, FinallyF) 
  	local Handled = false;
	local Success = true;
	local Message = '';
  	xpcall(
		function() TryF(); end,
		function(Msg)
			Success = false;
			Message = Msg;
			if ((type(ExceptF) == "function")) then
				Handled, Message = pcall(function() ExceptF(Msg); end);
			end;
		end);
  	if (type(FinallyF) == "function")	then
    	FinallyF();
  	end;
  	if not Success and not Handled then
    	error(Message, 1);
  	end;
end;

-- Strings

local String = string;

with(String, function() 

	function Concat(...)
		return table.concat({...});
	end;

end);

local StringMeta = GetMetaTable('');

with(StringMeta, function() 

	function __add(self, item)
		return String.Concat(self, tostring(item));
	end;
	
	function __index(self, index)
		if (type(index) == "number") then
			return self:sub(index, index);
		else
			return String[index];
		end;
	end;

end);

-- Main API 

with(HELLua_API, function() 

	Write = io['write'];

	function PrintTable(Table)
		print '{';
		for k,v in pairs(Table) do 
			print('  ', k, ' = ', v);
		end;
		print '};';
	end;
	
	function ShallowCopy(Table)
		local Result = {};
		for k,v in pairs(Table) do 
			Result[k] = v;
		end;
		return Result;
	end;
	
	function DeepCopy(Table, List, Name)
		if (not (type(List) == "table")) then 
			List = {};
		end;
		local Result = {};
		List[Table] = Result;
		for k,v in pairs(Table) do 
			if (not (type(v) == "table")) then 
				Result[k] = v;
			else
				if (not (type(List[v]) == "table")) then 
					Result[k] = DeepCopy(v, List);
				else
					Result[k] = List[v];
				end;
			end;
		end;
		local meta = GetMetaTable(Table);
		if ((type(meta) == "table")) then 
			local Meta = DeepCopy(meta);
			Result = SetMetaTable(Result, Meta);
		end;
		return Result;
	end;
	
	function TableHashCount(Table)
		local Result = 0;
		for k,v in pairs(Table) do (function() local A = Result; Result = Result + 1; return A; end)(); end;
		return Result;
	end;
	
	function ConcatByProperty(Table, Property, Glue)
		local Result = {};
		for k,v in pairs(Table) do 
			if (not (type(v[Property]) == "nil")) then 
				table.insert(Result, v[Property]);
			end;
		end;
		return table.concat(Result, Glue);
	end;

	function CreateWithEnvironment(StackLevel)
		local Environment = {};
		StackLevel = (((type(StackLevel) == "number")) and StackLevel or PreviousStackLevel);
		local OldEnv = getfenv(StackLevel);
		local EnvMeta = {};

		-- print(debug.traceback());

		function EnvMeta.__index(self, index)
			local Level = StackLevel;
			while (not (debug.getinfo(Level) == nil)) do
				local CountLevel = 1;
				local Name, Value = debug.getlocal(Level, CountLevel);
				while not (Name == nil) do
					if Name == index then return Value; end;
					CountLevel = CountLevel + 1;
					Name, Value = debug.getlocal(Level, CountLevel);
				end;
				Level = Level + 1;
			end;
			return OldEnv[index];
		end;

		function EnvMeta.__newindex(self, index, value)
			-- self[index] = value;
			rawset(self, index, value);
		end;

		return EnvMeta, OldEnv;
	end;

	function TableIsEmpty(Table)
		return (next(Table) == nil);
	end;

	function Pack(...)
		return {...};
	end;

	function CollideTablesByPointers(WeakTable, StrongTable)
		for k,v in pairs(StrongTable) do 
			WeakTable[k] = v;
		end;
		-- print 'Table Copied: ';
		-- PrintTable(WeakTable);
		return WeakTable;
	end;

	function CollideTablesSafe(WeakTable, StrongTable)
		WeakTable = DeepCopy(WeakTable);
		return CollideTablesByPointers(WeakTable, StrongTable); 
	end;

	function CollideTablesByProperty(WeakTable, StrongTable, Property)
		local Result = WeakTable;
		for k,v in pairs(StrongTable) do 
			if (not (type(Result[k]) == "table")) then 
				Result[k] = v;
			else 
				local Found = false;
				for k1, v1 in pairs(Result) do 
					if v1[Property] == v[Property] then 
						Result[k1] = v;
						Found = true;
						break;		
					end;
				end;
				if not Found then 
					table.insert(Result, v);
				end;
			end;
		end;
		return Result;
	end;

	function CollideTablesByPropertySafe(WeakTable, StrongTable, Property)
		WeakTable = DeepCopy(WeakTable);
		return CollideTablesByProperty(WeakTable, StrongTable, Property);
	end;

	function RandomString(Count, MinChar, MaxChar)
		local Result = {};
		if (not (type(MinChar) == "number")) then MinChar = 33; end;
		if (not (type(MaxChar) == "number")) then MaxChar = 126; end;
		if (MinChar > MaxChar) then 
			MinChar, MaxChar = MaxChar, MinChar; 
		end;
		for i = 1, Count do 
			Result[i] = string.char(math.random(MinChar, MaxChar));
		end;
		return table.concat(Result);
	end;

	function CreateCallableTable(func)
		local Result = {};
		return SetMetaTable(Result, {
			__call = func;
		});
	end;

	function _G.Import(Mask, From, To)
		for k,v in pairs(From) do 
			if ((type(k) == "string")) then 
				local PS, PF = k:find(Mask);
				if (PS == 1) and (PF == k:len()) then
					To[k] = v;
				end;
			end;
		end;
	end;

	function ExtractTablePointer(TableStr)
		local PS, PF, Pointer = TableStr:find('table: ([%da-fA-F]+)');
		return PS and Pointer or nil;
	end;

	function ExtractFunctionPointer(FunctionStr)
		local PS, PF, Pointer = FunctionStr:find('function: ([%da-fA-F]+)');
		return PS and Pointer or nil;
	end;

	function UseObjectWithFunction(Object, doFunction)
		local Environment = {};
		local OldEnv = GetEnvironment(PreviousStackLevel);
		local EnvMeta = {};

		function EnvMeta.__index(self, index)
			local PreResult = Object[index];
			if PreResult == nil then
				local Level = 2;
				while (not (debug.getinfo(Level) == nil)) do
					local CountLevel = 1;
					local Name, Value = debug.getlocal(Level, CountLevel);
					while not (Name == nil) do
						-- print(Level, CountLevel);
						-- print('Name: ', Name, '\tValue: ', Value);
						if Name == index then return Value; end;
						(function() local A = CountLevel; CountLevel = CountLevel + 1; return A; end)();
						Name, Value = debug.getlocal(Level, CountLevel);
					end;
					(function() local A = Level; Level = Level + 1; return A; end)();
				end;
				return OldEnv[index];
			else
				return PreResult;
			end;
		end;

		function EnvMeta.__newindex(self, index, value)
			Object[index] = value;
		end;

		Environment.__ENV = OldEnv;
		Environment.self = Object;
		Environment.this = Object;

		Environment = SetMetaTable(Environment, EnvMeta);

		return SetEnvironment(doFunction, Environment);
	end;

	function CopyFunction(Func)
		local Str = nil;
		Try(function() 
			Str = string.dump(Func);
		end , function(E) 
			error(('Cannot dump function: %s'):format(tostring(E)), 2);
		end);
		local FuncNew, Msg = loadstring(Str);
		if (not (type(FuncNew) == "function")) then error(('Cannot load dumped string. It is internal error: %s'):format(tostring(Msg))); end;
		return FuncNew;
	end;

end);

return HELLua_API;

