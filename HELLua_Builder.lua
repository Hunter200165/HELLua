 --[[ Sample script in HELLua ]]

--[[ Uses clause ]] 
	local math = require 'math'; 
	local debug = require 'debug';

--[[ Uses clause ]] 	
	local HL = dofile 'HELLua.lua';

local PathOut = 'Processed';
	
-- Init

_G.Try = HL.Try;
_G.with = HL.with;
_G.strictly_with = HL.strictly_with;
	
-- Bit module loader

local bit = nil;
Try(function()
	--[[ Uses clause ]] bit = require 'bit';
end , function(E) 
	Try(function() 
		--[[ Uses clause ]]  bit = require 'bit32';
	end , function(E1)
		-- Throw and raise are the same.
		error('No bit module is found.');
	end);
end);

-- Main module 

local Module = {
	LogFunction = print;
};

local ClearMsgs = false;
local VoidOutput = true;

local Msg_HeaderInitial = '[HELLua_Builder]';
local Msg_ExtensionChanged = Msg_HeaderInitial + '[Hint]: Extension has been changed: %s -> %s';

with(Module, function() 

	function Log(Msg, ...)
		LogFunction(Msg:format(...));
	end;
	
	local L = Log;

	function FixPath(path)
		path = path:gsub('^[%s/\\]+', ''):gsub('[%s/\\]+$', '');
		return path:gsub('/', '\\');
	end;

	function GetDirs(path)
		path = FixPath(path);
		local LastPos = 1;
		local Result = {};
		local PS, PF = path:find('\\');
		while (PS ~= nil) do 
			table.insert(Result, path:sub(LastPos, PF - 1));
			LastPos = PF + 1;
			PS, PF = path:find('\\', PF + 1);
		end;
		return Result, path:sub(LastPos);
	end;

	function BuildFile(From, To)
		From = (((type(From) == "string")) and From or (tostring(From)));
		if not ((type(To) == "string")) then 
			local PS, PF, extension = (tostring(From)):find('%.(.-)$');
			if (PS ~= nil) and (PS ~= 1) then 
				To = From:sub(1, PS - 1) + '.lua';
				Log(Msg_ExtensionChanged, extension, '.lua');
			end;
		end;
		return From, To;
	end;
	
	function Header()
		L 'HELLua Builder. Copyright (c) Hunter200165, 2018';
		L '';
	end;
	
	function Help()
		L 'Help for HELLua Builder:';
		L '  HELLua_Builder.lua <File in> [File out [Special settings]]'
	end;
	
	function ClearScr()
		os.execute 'cls';
	end;
	
	function FolderExists(Name)
		L ('** Scanning for folder existance: %s', Name);
		local res = os.execute('dir ' + Name + ((VoidOutput and (' > NUL') or (''))));
		L ('** Scanned: %s \n', tostring(res));
		if ClearMsgs then ClearScr(); end;
		return (res == 0);
	end;
	
	function CreateFolder(Name)
		L ('** Trying to create folder: %s', Name);
		local res = os.execute('mkdir ' + Name + ((VoidOutput and (' > NUL') or (''))));
		L ('** Tried: %s', tostring(res));
		if ClearMsgs then ClearScr(); end;
		return (res == 0);
	end;
	
	function Main(Args)
		if not ClearMsgs then Header(); end; 
		if not ((type((Args[1])) == "string")) then 
			Help();
			return;
		end;
		local From, To = BuildFile(Args[1], Args[2], Args[3]);
		local Path, Name = GetDirs(To);
		local PathStr = table.concat(Path, '\\');
		if not FolderExists(FixPath(PathStr + '\\' + PathOut)) then 
			if not CreateFolder(FixPath(PathStr + '\\' + PathOut)) then 
				error('Cannot create out folder [' + FixPath(PathStr + '\\' + PathOut) + '].');
			end;
		end;
		if ClearMsgs then Header(); end;
		To = FixPath(PathStr + '\\' + PathOut + '\\' + Name);
		From = FixPath(From); 
		L('Building file (%s -> %s)', tostring(From), tostring(To));
		HL.HMachine:BuildFile(From, To, ((Args[3] ~= 'false') and true or false));
		L 'Done.';
	end;
	
end);

-- 1 = In File
-- 2 = Out File
-- 3 = Also some settings

local Args = {...};
Module.Main(Args);


