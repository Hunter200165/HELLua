--[[
	HELLua written by Hunter200165;
	HELLua - HEnhanced Lexem Lua compiler
]]

local HELLua = {};

local Bit = require 'bit';
local raise = error;

local RTErrorMask = 'HELLua(RunTime)[FATAL]: %s';

local VarMask = '([%a%_][%w%d%_%.%[%]]*)';
local VarNameMask = ('([%a%_][%w]*)');
local ValueMask = '([%w%_%#%.%[%]]+)';
local FunctionMask = '([%a%_][%w%_%.:%[%]]*%s-%b())';
local QuotientMask = '(%b())';
local TableMask = '(%b{})';
local SpaceMask = '%s-';
local SpaceMaskBig = '%s+';
local StringMask = '(\'.-[^\\]\')';
local StringMaskAnother = '(".-[^\\]")';
local StringMaskSimple = '(%b\'\')';
local StringMaskAnotherSimple = '(%b"")';
local StringMaskSquare = '%[(%=-)%[.-%](%1)%]';

local CommentMultiLineMask = '%-%-%[(%=-)%[.-%](%1)%]';
local CommentSingleLineMask = '%-%-.-\n';

local Debug = true;

-- Miscs

local type = type;
local dofile = dofile;
local tostring = tostring;
local tonumber = tonumber;
local _G = _G;
local string = string;
local table = table;

local NOT = Bit.bnot;
local AND = Bit.band;
local OR = Bit.bor;
local XOR = Bit.bxor;
local SHL = Bit.lshift;
local SHR = Bit.rshift;

function HELLua.IterateAll(Table, String, Count)
	local Len = #Table;
	local Counter = {};
	local Result = {};
	for i = 1, Count do Counter[i] = 1; end;
	local Check = false;
	while not Check do
		local Now = {};
		for i = 1, Count do
			table.insert(Now, Table[Counter[i]]);
		end;
		table.insert(Result, String:format(unpack(Now)));
		local Flag = false;
		for i = 1, Count do
			if i == 1 then
				Counter[i] = Counter[i] + 1;
			end;
			if Flag then
				Counter[i] = Counter[i] + 1;
				Flag = false;
			end;
			if Counter[i] > Len then
				Counter[i] = (Counter[i] - 1) % Len + 1;
				Flag = true;
			end;
		end;
		Check = Flag;
	end;
	return Result;
end;

function HELLua.IntNOT(Content)
	local Result = AND( NOT(Content), IntegerMask );
	return Result;
end;

local IntNOT = HELLua.IntNOT;

function HELLua.DeepCopy(Array)
	local Result = {};
	for k,v in pairs(Array) do
		if not (type(v) == 'table') then
			Result[k] = v;
		else
			Result[k] = HELLua.DeepCopy(v);
		end;
	end;
	local meta = getmetatable(Array);
	if type(meta) == 'table' then
		Result = setmetatable(Result, HELLua.DeepCopy(meta));
	end;
	return Result;
end;

function HELLua.PreConcat(Table, Str)
	local Result = {};
	for k,v in pairs(Table) do
		Result[k] = Str + v;
	end;
	return Result;
end;

function HELLua.PostConcat(Table, Str)
	local Result = {};
	for k,v in pairs(Table) do
		Result[k] = tostring(v) + Str;
	end;
	return Result;
end;

function HELLua.ShallowCopy(Array)
	local Result = {};
	for k,v in pairs(Array) do
		Result[k] = v;
	end;
	return Result;
end;

function HELLua.RunTimeError(ErrorMsg, ...)
	raise(RTErrorMask:format(ErrorMsg:format(...)));
end;

function HELLua.Log(Msg, ...)
	if Debug then
		print(Msg:format(...));
	end;
end;

local RunTimeError = HELLua.RunTimeError;
local Log = HELLua.Log;

function HELLua.Record(Table)
	local Array = HELLua.DeepCopy(Table);
	Array.Create = function()
		return HELLua.DeepCopy(Array);
	end;
	Array.Copy = function(self)
		return HELLua.DeepCopy(self);
	end;

	return Array;
end;

local Record = HELLua.Record;

function HELLua.GetRAMUsage()
	return (collectgarbage 'count');
end;

local GetRAMUsage = HELLua.GetRAMUsage;

function HELLua.ClearRAM()
	local Was = GetRAMUsage();
	collectgarbage 'collect';
	local Is = GetRAMUsage();
	return Was - Is;
end;

--[[ String ]]

function string:concat(...)
	return table.concat({self, ...});
end;

function string:ReplaceString(Start, Finish, Replacement)
	local First = self:sub(1, Start - 1);
	local Last = self:sub(Finish + 1);
	local Result = First:concat(Replacement, Last);
	First, Last = nil, nil;
	HELLua.ClearRAM();
	return Result;
end;

function string:PreConcat(Table)
	return HELLua.PreConcat(Table, self);
end;

function string:PostConcat(Table)
	return HELLua.PostConcat(Table, self);
end;

getmetatable('').__add = function(self, value)
	return self:concat(value);
end;

getmetatable('').__index = function(self, index)
	if (type(index) == 'number') then
		return self:sub(index, index);
	else
		return string[index];
	end;
end;

-- Record

HELLua.TMacros = Record {
	-- Mask of Macros
	Mask = '';
	ReplaceFunction = function() end;
	CheckFunction = function() end;
	RequiredMacroses = {};
};

HELLua.TScope = Record {
	Mask = '';

	Start = 0;
	Finish = 0;
};

HELLua.TEnhancedLexeme = Record {
	Macroses = {};
	Scopes = {};
	Header = {};
};

HELLua.TLua = Record {
	Lexeme = HELLua.TEnhancedLexeme.Create();

	Lines = {};

	-- Allow macros to check code?
	AllowInternalChecks = true;
	-- Allow warning even on rude syntax errors? (DO NOT RECOMMENDED!)
	WarningsInsteadOfErrors = false;

	-- Allow simple optimizations? For example `A & 0` will be `0`, but not `bit.band(A, 0)`
	AllowSimpleCodeOptimization = false;
	-- Allow Hints?
	AllowHints = true;
	-- Allow Warnings?
	AllowWarnings = true;

	-- Allow Directives? Something like `--${MACRO_PASCAL ON}` will turn on `:=` assignment and `=` equality
	AllowDirectives = true;
	-- Allow Line counting? Then all [[%d+]] will be replaced with line count. Slowering a bit. Helping - over infifnity.
	AllowLinesCounting = true;
};

HELLua.TList = Record {
	-- Simple list for HELLua
};

local TLua = HELLua.TLua;
local TEnhancedLexeme = HELLua.TEnhancedLexeme;
local TMacros = HELLua.TMacros;
local TList = HELLua.TList;
local TScope = HELLua.TScope;

local OpeningScopes = {
	-- All scopes that needs to be finished with `end`
	'do';
	'function';
	'if';
	-- 'for'; --[[ do is presented ]]
	-- 'while'; --[[ do is presented ]]
	insert = function(self, value)
		self[#self + 1] = value;
		return #self;
	end;
	In = function(self, value)
		for i = 1, #self do
			if self[i] == value then
				return true, i;
			end;
		end;
		return false, nil;
	end;
};

--[[ TScope ]]

function TScope.New(Keyword, Mask, Callback)

	local Result = TScope.Create();
	Result.Mask = Mask;
	Result.Keyword = Keyword;
	Result.Callback = Callback;

	return Result;

end;

function TScope.ResolvePositions(Str, Positions)
	local Result = {};
	Result.InitialString = Str:sub(Positions.positionStartBefore, Positions.positionStart);
	Result.Body = Str:sub(Positions.positionStart --[[ + 1 ]], Positions.positionEndFirst - 1);
	Result.EndedBody = Str:sub(Positions.positionStart, Positions.positionEndSecond);
	Result.Full = Str:sub(Positions.positionStartBefore, Positions.positionEndSecond);

	return Result;
end;

--[[ TList ]]

function TList.New(Arr)
	local Result = TList.Create();
	for i = 1, #Arr do
		Result:Add(Arr[i]);
	end;

	setmetatable( Result, {
		__add = function(self, additional)
			if type(additional) == 'table' then
				local Result = TList.New(self);
				for i = 1, #additional do
					Result:Add(additional[i]);
				end;
				return Result;
			end;
		end;
	} );

	return Result;
end;

function TList:IsIn(value)
	for i = 1, #self do
		if self[i] == value then
			return true, i;
		end;
	end;
	return false, nil;
end;

function TList:Add(value)
	local Index = #self + 1;
	self[Index] = value;
	return Index;
end;

function TList:RemoveAt(index)
	self[index] = nil;
end;

function TList:Clear()
	-- Safe and fast!
	for i = 1, #self do
		self[i] = nil;
	end;
end;

--[[ TMacros ]]

function TMacros.New(pattern, replace, check)
	local Result = TMacros.Create();
	Result.Mask = pattern;
	Result.ReplaceFunction = replace;
	Result.CheckFunction = check;
	return Result;
end;

--HELLua.Macro_AndC = Macro_AndC;

--[[ TEnhancedLexeme ]]

function TEnhancedLexeme.Init()
end;

function TEnhancedLexeme:Add(...)
	local Arr = {...};
	for Index, Macro in pairs(Arr) do
		table.insert( self.Macroses, Macro );
	end;
end;

function TEnhancedLexeme:AddScope(...)
	local Arr = {...};
	for Index, Macro in pairs(Arr) do
		table.insert( self.Scopes, Macro );
	end;
end;

function TEnhancedLexeme:AddHeader(Str)
	table.insert( self.Header, Str );
end;

--[[ TLua ]]

TLua.Lexeme = TEnhancedLexeme.Create();

function TLua:Msg(Str, ...)
	Str = Str:format(unpack {...});
	if self.AllowLinesCounting then
		Str = Str:gsub('%[%[(%d+)%]%]', function(num)
			local Numb = tonumber(num);
			if Numb == nil then
				return ('[[' + num + ']]');
			end;
			return ('[%s](Line: %s)'):format(num, TLua.ResolveLineByPosition(self.Lines, Numb));
		end);
	end;
	print(Str);
end;

function TLua.ScanScope(Str, Position, Mask, ScopesPairs, OpenedScope)
	--[[
		Scope pair is:
		{
			Open = 'do';
			Close = 'end';
		}
	]]
	local function ScopeOpens(Begin)
		for i = 1, #ScopesPairs do
			if ScopesPairs[i].Open == Begin then
				return true, ScopesPairs[i], i;
			end;
		end;
		return false, nil, -1;
	end;

	local Closing = TList.Create();
	local Pos = 0;

	local function ScopeAdd(OpenScope)
		local Success, Pair, Index = ScopeOpens(OpenScope);
		if not Success then
			return false;
		end;
		if not ((type(Pair.Close) == "string") or (type(Pair.Close) == 'table')) or (Pair.Close == '') then
			RunTimeError('Pair with open symbol "%s" has invalid closing character.', OpenScope);
		end;
		Closing:Add({Pair.Open, Pair.Close, Pos});
		return true;
	end;

	local function ScopeClose(Word)
		--local Closed = (Word == Closing[#Closing][2]);
		local Cur = Closing[#Closing];
		local Closed = false;
		if type(Cur[2]) == 'table' then
			for i = 1, #Cur[2] do
				if Cur[2][i] == Word then
					Closed = true;
					break;
				end;
			end;
		else
			Closed = (Word == Cur[2]);
		end;
		if Closed then
			table.remove(Closing, #Closing);
		end;
		return Closed;
	end;

	if not ScopeAdd(OpenedScope) then
		RunTimeError('Initial scope is invalid.');
	end;

	local PS, PF = Str:find(Mask, Position);
	while (PS ~= nil) and (#Closing > 0) do
		local StrCopied = Str:sub(PS, PF);
		local Fnd = ScopeClose(StrCopied);
		if not Fnd then
			if ScopeAdd(StrCopied) then Pos = PS; end;
		else
			if #Closing < 0 then RunTimeError 'Scope index is out of bounds.'; end;
			if #Closing == 0 then break; end;
		end;
		PS, PF = Str:find(Mask, PF + 1);
	end;

	if #Closing > 0 then
		RunTimeError('Scope "%s" is not closed at [[%d]].', Closing[#Closing][1], Closing[#Closing][3]);
	end;

	return PS, PF;

end;

function TLua.ParseForScopes(Str, Start, IsValidPositionFunction)
	local Result = {positionStart = Start; positionEndFirst = Start; positionEndSecond = Start};
	local Opened = {{Start = Start; Finish = Start}};
	local LastScope = Start;
	local positionStart, positionEnd, word = (Str):find('([%w%_]+)', Start);
	if positionStart == nil then return nil; end;
	while positionStart ~= nil do
		-- Log('Found scope word: %d, %d, %s', positionStart, positionEnd, word);
		if not IsValidPositionFunction(positionStart) or not IsValidPositionFunction(positionEnd) then
			-- Nothing is here.
			-- Simply skip this loop.
		else
			if OpeningScopes:In(word) then
				-- Log('Scope opened: %s', word);
				table.insert(Opened, {Start = positionStart});
				LastScope = positionStart;
			elseif word == 'end' then
				-- Log('Scope closed.');
				-- Opened = Opened - 1;
				table.remove(Opened, #Opened);
			end;
			if #Opened == 0 then
				Result.positionEndFirst = positionStart;
				Result.positionEndSecond = positionEnd;
				break;
			elseif #Opened < 0 then
				HELLua.RunTimeError('Scope index is out of bounds.');
			end;
		end;
		positionStart, positionEnd, word = (Str):find('([%w%_]+)', positionEnd + 1);
	end;
	if #Opened > 0 then
		HELLua.RunTimeError('Scope is not closed at [[%d]]. Last scope is [[%d]]. String: %s', Start, Opened[#Opened].Start, '');
	end;
	return Result;
end;

function TLua.ParseStringForPatterns(Str, Pattern, Func)
	local FoundStart, FoundFinish = Str:find(Pattern);
	local From = FoundFinish;
	while FoundStart ~= nil do
		From, StrRet = Func(FoundStart, FoundFinish);
		From = From or FoundFinish + 1;
		Str = StrRet or Str;
		FoundStart, FoundFinish = Str:find(Pattern, From);
	end;
end;

function TLua.ScanPositions(Str)

	-- Consts

	-- Vars

	local Result = {};
	Result.Strings = TList.Create();
	Result.Comments = TList.Create();
	local SLine = false;
	local MLine = false;
	local SComment = false;
	local MComment = false;
	local ASymbols = '';
	local AStrSymbol = '';

	local StartPosition  = 0;
	local StartPositionCopy = nil;
	local FinishPosition = nil;

	local PStart, PFinish, Word = Str:find('([%-%[%]%\'%"%=%s\\]+)');
	while PStart ~= nil do
		-- Log(' I am on %s - %s: %s', PStart, PFinish, Word);
		if SLine then
			-- Log('  Str Symbol: %s ', AStrSymbol);
			local PS, PF, Slashes = (Word):find('([%\\]-)%' + AStrSymbol);
			-- Log('  T: %s, %s, %s', tostring(PS), tostring(PF), tostring(Slashes));
			while PS ~= nil do
				-- Log('  Slashes: %s', Slashes);
				local Closed = (Slashes:len() % 2) == 0;
				if Closed then
					FinishPosition = PStart + PF - 1;
					-- Log('  Position of SLine is %s, %s;', StartPosition, FinishPosition);
					Result.Strings:Add({Start = StartPosition; Finish = FinishPosition});
					SLine = false;
					break;
				end;
				PS, PF, Slashes = (Word):find('([%\\]-)%' + AStrSymbol, PF + 1);
			end;
		elseif MLine then
			local PS, PF, Symbols = Word:find('%](=-)%]');
			local Symbol = nil;
			while PS ~= nil do
				local Closed = (Symbols == ASymbols);
				if Closed then
					FinishPosition = PStart + PF - 1;
					Result.Strings:Add({Start = StartPosition; Finish = FinishPosition});
					MLine = false;
					break;
				end;
				PS, PF, Symbols = Word:find('%](=-)%]', PF + 1);
			end;
		elseif SComment then
			local PS, PF = Word:find('\n');
			if PS ~= nil then
				FinishPosition = PStart + PF - 1;
				Result.Comments:Add({Start = StartPosition; Finish = FinishPosition});
				SComment = false;
			end;
		elseif MComment then
			local PS, PF, Symbols = Word:find('%](=-)%]');
			while PS ~= nil do
				local Closed = (Symbols == ASymbols);
				if Closed then
					FinishPosition = PStart + PF - 1;
					Result.Comments:Add({Start = StartPosition; Finish = FinishPosition});
					MComment = false;
					break;
				end;
				PS, PF, Symbols = Word:find('%](=-)%]', PF + 1);
			end;
		else
			-- Will find the truth
			-- Multiline comment
			local PS, PF, Symbols = Word:find('%-%-%[(=-)%[');
			if PS ~= nil then
				StartPosition = PS + PStart - 1;
				StartPositionCopy = PS + PStart - 1;
				ASymbols = Symbols;
				MComment = true;
			else
				-- Single line comment
				PS, PF = Word:find('%-%-');
				if PS ~= nil then
					StartPosition = PS + PStart - 1;
					StartPositionCopy = PS + PStart - 1;
					SComment = true;
				else
					-- Multiline string
					PS, PF, Symbols = Word:find('%[(=-)%[');
					if PS ~= nil then
						StartPosition = PS + PStart - 1;
						StartPositionCopy = PS + PStart - 1;
						ASymbols = Symbols;
						MLine = true;
					else
						-- Single line string;
						PS, PF, Symbol = Word:find('([%"%\'])');
						if PS ~= nil then
							AStrSymbol = Symbol;
							StartPosition = PS + PStart - 1;
							StartPositionCopy = PS + PStart - 1;
							SLine = true;
						end;
					end;
				end;
			end;
		end;
		if FinishPosition ~= nil then
			PStart, PFinish, Word = Str:find('([%-%[%]%\'%"%=%s\\]+)', FinishPosition + 1);
			FinishPosition = nil;
		elseif StartPositionCopy ~= nil then
			PStart, PFinish, Word = Str:find('([%-%[%]%\'%"%=%s\\]+)', StartPositionCopy + 1);
			StartPositionCopy = nil;
		else
			PStart, PFinish, Word = Str:find('([%-%[%]%\'%"%=%s\\]+)', PFinish + 1);
		end;
	end;

	local ErrorMsg = '%s is not closed from %s';

	if SComment then
		Result.Comments:Add({Start = StartPosition; Finish = Str:len()});
	end;
	if SLine then
		RunTimeError(ErrorMsg, 'singleline line', StartPosition);
	elseif MLine then
		RunTimeError(ErrorMsg, 'multiline string', StartPosition);
	elseif MComment then
		RunTimeError(ErrorMsg, 'multiline comment', StartPosition);
	end;

	return Result;

end;

function TLua.TestScan(Text)
	Log '** Test scan for positions';
	local List = TLua.ScanPositions(Text);
	Log ' * Strings';
	for i = 1, #List.Strings do
		Log('  %d: {Start = %s; Finish = %s; String = %s};', i, List.Strings[i].Start, List.Strings[i].Finish, Text:sub(List.Strings[i].Start, List.Strings[i].Finish));
	end;
	Log ' * Comments';
	for i = 1, #List.Comments do
		Log('  %d: {Start = %s; Finish = %s; Comment = %s};', i, List.Comments[i].Start, List.Comments[i].Finish, Text:sub(List.Comments[i].Start, List.Comments[i].Finish));
	end;
	Log '** Done';
end;

function TLua.ParseForLines(Text)
	-- Log 'Scanning for lines...';
	local Result = TList.Create();
	local PrevPos = 0;
	local PS, PF = Text:find('.-\n');
	while not (PS == nil) do
		Result:Add { Start = PS; Finish = PF };
		PrevPos = PF + 1;
		PS, PF = Text:find('.-\n', PF + 1);
	end;
	Result:Add { Start = PrevPos; Finish = Text:len() };
	return Result;
end;

function TLua.ResolveLineByPosition(Lines, Position)
	local Line = -1;
	for i = 1, #Lines do
		-- Log('  LPosition: %s >> Start: %s >> Finish: %s', Position, Lines[i].Start, Lines[i].Finish);
		if (Position >= Lines[i].Start) and (Position <= Lines[i].Finish) then
			Line = i;
			break;
		end;
	end;
	return Line;
end;

function TLua:ProcessText(Text)
	local Result = Text;
	-- Concat with space and new line
	Result = ' ' + Result + '\n';
	local Strings = {};
	local Lines = self.Lines;
	local Comments = TList.Create();

	local CommentConstant = 1;
	local StringConstant = 2;

	local function InComments(position)
		local Result = false;
		local Index = nil;
		for i = 1, #Comments do
			if (position >= Comments[i].Start) and (position <= Comments[i].Finish) then
				Result = true;
				Index = i;
				break;
			end;
		end;
		return Result, Index;
	end;

	local function InString(position)
		local Result = false;
		local Index = nil;
		for i = 1, #Strings do
			v = Strings[i];
			if (position >= v.Start) and (position <= v.Finish) then
				Result = true;
				Index = i;
				break;
			end;
		end;
		return Result, Index;
	end;

	local function IsValidPosition(position)
		-- Will return true, if position is not in comment or in string.
		local InS, SPos = InString(position);
		if InS then 
			return false, SPos, StringConstant;
		end;
		local InC, CPos = InComments(position);
		if InC then 
			return false, CPos, CommentConstant;
		end;
		return true;
	end;

	local function SearchAll()
		local List = TLua.ScanPositions(Result);
		Comments = List.Comments;
		Strings = List.Strings;
		if self.AllowLinesCounting then
			self.Lines = TLua.ParseForLines(Result);
			-- self.Text = Result;
		end;
	end;

	SearchAll();

	local PassScope = function(Current, Scope)
		TLua.ParseStringForPatterns(Result, Current, function(Start, Finish)
			-- if (not IsValidPosition(Start) or not IsValidPosition(Finish)) then
			-- 	return nil, nil;
			-- end;
			local NInA, IndA, TypeA = IsValidPosition(Start);
			if not NInA then 
				if TypeA == StringConstant then 
					return Strings[IndA].Finish + 1, nil;
				elseif TypeA == CommentConstant then 
					return Comments[IndA].Finish + 1, nil;
				else 
					return nil, nil; 
				end;
			end;
			local NInA, IndA, TypeA = IsValidPosition(Finish);
			if not NInA then 
				return Start + 1, nil;
			end;
			local Len = Finish - Start + 1;
			local Positions = TLua.ParseForScopes(Result, Finish + 1, IsValidPosition);
			if Positions == nil then
				return nil, nil;
			end;
			Positions.positionStartBefore = Start;
			local Rpl = Scope:Callback(Result, Positions, IsValidPosition);
			local NewLen = Rpl:len();
			Result = Result:ReplaceString(Start, Positions.positionEndSecond, Rpl);
			-- Log('  ^ Scope: %s; Replace: %s ', Current, Rpl);
			SearchAll();
			return Start, Result;
		end);
	end;

	local PassMacros = function(Current, Macros)
		local AllowStringEnd = Current:match('"%)%s*$') ~= nil;
		if not AllowStringEnd then AllowStringEnd = Current:match('\'%)%s*$') ~= nil; end;
		local AllowStringStart = Current:match('^%s*%(%%b"') ~= nil;
		if not AllowStringStart then AllowStringStart = Current:match('^%s*%(%%b\'') ~= nil; end;
		TLua.ParseStringForPatterns(Result, Current, function(Start, Finish)
			-- if (not IsValidPosition(Start) or not IsValidPosition(Finish)) then
			-- 	return nil, nil;
			-- end;
			local NInA, IndA, TypeA = IsValidPosition(Start);
			if not NInA then 
				if TypeA == StringConstant then 
					if not AllowStringStart then 
						return Strings[IndA].Finish + 1, nil;
					end;
				elseif TypeA == CommentConstant then 
					return Comments[IndA].Finish + 1, nil;
				else 
					return nil, nil; 
				end;
			end;
			local NInA, IndA, TypeA = IsValidPosition(Finish);
			if not NInA then 
				if not AllowStringEnd or (TypeA ~= StringConstant) then 
					return Start + 1, nil;
				end;
			end;
			local Len = Finish - Start + 1;
			local MRFunc = setfenv(Macros.ReplaceFunction, setmetatable({Machine = self}, {__index = _G, __newindex = _G}));
			-- local NFinish, ResString = Macros.ReplaceFunction(Macros, Result:sub(Start, Finish), Current, Result, Start, Finish);
			local NFinish, ResString, StartScan = MRFunc(Macros, Result:sub(Start, Finish), Current, Result, Start, Finish);
			if (type(ResString) ~= 'string') then
				ResString = NFinish;
				NFinish = nil;
			end;
			if type(NFinish) == 'number' then
				Finish = NFinish;
			end;
			if not (type(StartScan) == 'number') then 
				StartScan = Start;
			end;
			local NewLen = ResString:len();
			Result = Result:ReplaceString(Start, Finish, ResString);
			-- Log('  ^ Macro: %s; Replace: %s ', Current, ResString);
			-- Log('String: %s', Result);
			SearchAll();
			return StartScan, Result;
		end);
	end;

	local function ParseBoolDirectives()
		TLua.ParseStringForPatterns(Result, '%-%-%$(%b{})\n', function(Start, Finish)
			local Str = Result:sub(Start, Finish);
			local _, _, Dir = Str:find('(%b{})\n');
			-- Log('Found Directive: %s -- %s', Str, Dir);
			if InString(Start) or InString(Finish) then --[[Log 'In string';]] return nil, nil; end;
			local IComment, ICPos = InComments(Start);
			if IComment then
				if not (Start == Comments[ICPos].Start) then
					--[[Log ('In comment: %s != %s', Start, Comments[ICPos].Start)]]
					return nil, nil;
				end;
			end;
			local PS, PF, Instruction, Toogle = Dir:find('([%w%_]+)%s+([%w%+%-]+)');
			-- Log('Instruction, Toogle = %s, %s', Instruction, Toogle);
			if PS == nil then return nil, nil; end;
			Instruction = Instruction:upper();
			Toogle = Toogle:upper();
			local BoolT = false;
			if (Toogle == 'ON') or (Toogle == '+') then
				BoolT = true;
			elseif (Toogle == 'OFF') or (Toogle == '-') then
				BoolT = false;
			else
				if self.AllowHints then
					self.Msg('[Compiling][Hint]: Directive "%s" has invalid toogle flag "%s".', tostring(Instruction), tostring(Toogle));
				end;
				return nil, nil;
			end;
			if Instruction == 'HINTS' then
				self.AllowHints = BoolT;
			elseif Instruction == 'WARNINGS' then
				self.AllowWarnings = BoolT;
			elseif Instruction == 'NOERRORS' then
				self.WarningsInsteadOfErrors = BoolT;
			elseif Instruction == 'MODE_PASCAL' then
				if BoolT and not self.PascalMode then
					if self.AllowHints then
						self:Msg('[Compiler][Hint]: Pascal mode is turned on.');
					end;
					for i = #HELLua.MacrosesPascal, 1, -1 do
						table.insert(self.Lexeme.Macroses, 1, HELLua.MacrosesPascal[i]);
					end;
					self.PascalMode = true;
				else
					if self.PascalMode then
						for i = 1, #HELLua.MacrosesPascal do
							table.remove(self.Lexeme.Macroses, 1);
						end;
					end;
				end;
			elseif Instruction == 'OPTIMIZATION' then
				self.AllowSimpleCodeOptimization = BoolT;
			elseif Instruction == 'CHECKS' then 
				self.AllowInternalChecks = BoolT;
			else
				if self.AllowHints then
					self.Msg('[Compiling][Hint]: Directive "%s" is not resolved.', tostring(Instruction));
				end;
				return nil, nil;
			end;
		end);

		TLua.ParseStringForPatterns(Result, '%-%-%$(%b{})$', function(Start, Finish)
			local Str = Result:sub(Start, Finish);
			local _, _, Dir = Str:find('%-%-%$(%b{})$');
			-- Log('Found Directive: %s -- %s', Str, Dir);
			if InString(Start) or InString(Finish) then return nil, nil; end;
			local IComment, ICPos = InComments(Start);
			if IComment then
				if not ((Start ~= Comments[ICPos].Start) or (Finish ~= Comments[ICPos].Finish)) then return nil, nil; end;
			else
				IComment, ICPos = InComments(Finish);
				if IComment then
					if not ((Start ~= Comments[ICPos].Start) or (Finish ~= Comments[ICPos].Finish)) then return nil, nil; end;
				end;
			end;
			local PS, PF, Instruction, Toogle = Dir:find('([%w%_]+)%s+([%w%+%-]+)');
			if PS == nil then return nil, nil; end;
			Instruction = Instruction:upper();
			Toogle = Toogle:upper();
			local BoolT = false;
			if (Toogle == 'ON') or (Toogle == '+') then
				BoolT = true;
			elseif (Toogle == 'OFF') or (Toogle == '-') then
				BoolT = false;
			else
				if self.AllowHints then
					self.Msg('[Compiling][Hint]: Directive "%s" has invalid toogle flag "%s".', tostring(Instruction), tostring(Toogle));
				end;
				return nil, nil;
			end;
			if Instruction == 'HINTS' then
				self.AllowHints = BoolT;
			elseif Instruction == 'WARNINGS' then
				self.AllowWarnings = BoolT;
			elseif Instruction == 'NOERRORS' then
				self.WarningsInsteadOfErrors = BoolT;
			elseif Instruction == 'MODE_PASCAL' then
				if BoolT and not self.PascalMode then
					self.PascalMode = true;
					if self.AllowHints then
						self.Msg('[Compiler][Hint]: Pascal mode is turned on.');
					end;
					for i = #HELLua.MacrosesPascal, 1, -1 do
						table.insert(self.Lexeme.Macroses, 1, HELLua.MacrosesPascal[i]);
					end;
				elseif not BoolT and self.PascalMode then
					self.PascalMode = false;
					if self.AllowHints then
						self.Msg('[Compiler][Hint]: Pascal mode is turned off.');
					end;
					for i = 1, #HELLua.MacrosesPascal do
						table.remove(self.Lexeme.Macroses, 1);
					end;
				end;
			elseif Instruction == 'OPTIMIZATION' then
				self.AllowSimpleCodeOptimization = BoolT;
			elseif Instruction == 'LINECOUNTING' then
				self.AllowLinesCounting = BoolT;
			else
				if self.AllowHints then
					self.Msg('[Compiling][Hint]: Directive "%s" is not resolved.', tostring(Instruction));
				end;
				return nil, nil;
			end;
		end);
	end;

	if self.AllowDirectives then
		ParseBoolDirectives();
	end;

	for k,v in pairs(self.Lexeme.Macroses) do
		local Current = v.Mask;
		local Typ = type(Current);
		if Typ == 'string' then
			PassMacros(Current, v);
		elseif Typ == 'table' then
			for k1, v1 in pairs(Current) do
				PassMacros(v1, v);
			end;
		end;
	end;

	for k,v in pairs(self.Lexeme.Scopes) do
		local Current = v.Mask;
		local Typ = type(Current);
		if Typ == 'string' then
			PassScope(Current, v);
		else
		end;
	end;

	--[[ Checks ]]

	if self.AllowInternalChecks then

		for k,v in pairs(self.Lexeme.Macroses) do
			local CheckFunc = v.CheckFunction;
			if (type(CheckFunc) == 'function') then
				local Success, Msg = pcall(function()
					CheckFunc(Result, self.WarningsInsteadOfErrors, IsValidPosition, self);
				end);
				if not Success then
					if self.WarningsInsteadOfErrors then
						if self.AllowLinesCounting then
							Msg = Msg:gsub('%[%[(%d+)%]%]', function(num)
								local Numb = tonumber(num);
								if Numb == nil then
									return ('[[' + num + ']]');
								end;
								return ('[%s](Line: %s)'):format(num, TLua.ResolveLineByPosition(self.Lines, Numb));
							end);
						end;
						self.Msg('[Compiling::Check][Error]: %s', tostring(Msg));
					else
						if self.AllowLinesCounting then
							Msg = Msg:gsub('%[%[(%d+)%]%]', function(num)
								local Numb = tonumber(num);
								if Numb == nil then
									return ('[[' + num + ']]');
								end;
								return ('[%s](Line: %s)'):format(num, TLua.ResolveLineByPosition(self.Lines, Numb));
							end);
						end;
						RunTimeError('\n[Compiling::Check][Error]: %s', tostring(Msg));
					end;
				end;
			end;
		end;

	end;

	return Result;
end;

function TLua:BuildFile(FInName, FOutName, Immutable)
	local Self = self;
	if not Immutable then
		Self = HELLua.DeepCopy(self);
	end;
	FOutName = FOutName or FInName + '.lua';
	local FIn, msg_in = io.open(FInName, 'r');
	local FOut, msg_out = io.open(FOutName, 'w');
	if not (FIn or FOut) then
		pcall(function() FIn:close(); end);
		pcall(function() FOut:close(); end);
		RunTimeError('Could not load input files (%s -> %s): %s', FInName, FOutName, msg_in or msg_out);
	end;
	-- Read all text
	local TextOf = FIn:read('*a');
	local Parsed = nil;
	local Success, Msg = pcall(function() Parsed = Self:ProcessText(TextOf); end);
	if not Success then
		pcall(function() FIn:close(); end);
		pcall(function() FOut:close(); end);
		if Self.AllowLinesCounting then
			Msg = Msg:gsub('%[%[(%d+)%]%]', function(num)
				local Numb = tonumber(num);
				if Numb == nil then
					return ('[[' + num + ']]');
				end;
				return ('[%s](Line: %s)'):format(num, TLua.ResolveLineByPosition(Self.Lines, Numb));
			end);
		end;
		RunTimeError('Build time error (%s -> %s): %s', tostring(FInName), tostring(FOutName), tostring(Msg));
	end;
	FOut:write(Parsed);
	pcall(function() FIn:close(); end);
	pcall(function() FOut:close(); end);
end;

-- Macroses

local ValueOrFunction = {FunctionMask, ValueMask, QuotientMask, TableMask, StringMaskSimple, StringMaskAnotherSimple};
local VarValueOrFunction = {VarMask, ValueMask, FunctionMask, QuotientMask}

local Check_VarsMsg_Pure = '(variable, number, brackets) are the only arguments. Strings and tables are not supported - write them in brackets.';
local Check_VarsMsg = Check_VarsMsg_Pure + '\n';

local Compiling_Hint_Optimization = '[Compiling][Hint]: Operator "%s" is optimized to value %s';

-- Scopes

local ExceptScope = TScope.New(
	'except',
	'except%s+on%s+' + VarMask + '%s+do',
	function(self, StrObject, PositionObject)
		local PList = TScope.ResolvePositions(StrObject, PositionObject);
		local _, _, Var = PList.InitialString:find(self.Mask); -- Will find variable name!
		return (', function(%s)%send)'):format(Var, PList.Body) ;
	end);

local FinallyScope = TScope.New(
	'finally',
	'finally%s+do',
	function(self, StrObject, PositionObject)
		local PList = TScope.ResolvePositions(StrObject, PositionObject);
		return (', nil, function()%send)'):format(PList.Body) ;
	end);

local StriclyWithScope = TScope.New(
	'with',
	'strictly%s+with%s+' + VarMask + '%s+do',
	function(self, StrObject, PositionObject)
		local PList = TScope.ResolvePositions(StrObject, PositionObject);
		local _, _, Var = PList.InitialString:find(self.Mask); -- Will find variable name!
		return ('strictly_with(%s, function()%send)'):format(Var, PList.Body) ;
	end);

local WithScope = TScope.New(
	'with',
	'with%s+' + VarMask + '%s+do',
	function(self, StrObject, PositionObject)
		local PList = TScope.ResolvePositions(StrObject, PositionObject);
		local _, _, Var = PList.InitialString:find(self.Mask); -- Will find variable name!
		return ('with(%s, function()%send)'):format(Var, PList.Body) ;
	end);

local ForLoopC_Scope = TScope.New(
	'for',
	'for%s-%((.-);(.-);(.-)%)%s-do',
	function(self, StrObject, PositionObject)
		local PList = TScope.ResolvePositions(StrObject, PositionObject);
		local _, _, Initial, Condition, DoPart = PList.InitialString:find(self.Mask); -- Will find variable name!
		-- Log(' C For Loop Body : %s ', PList.Body);
		-- Log(' C For Loop Initial: %s ', Initial);
		-- Log(' C For Loop Condition: %s ', Condition);
		-- Log(' C For Loop Do Part: %s ', DoPart);
		return ('do %s; while (true) do if not (%s) then break; end; %s %s end; end'):format(Initial, Condition, PList.Body, DoPart);
	end
);

local ClassScopeInherited = TScope.New(
	'class',
	'class(%s+)' + VarNameMask + '(%s+)inherits(%s+)from(%s+)' + VarNameMask + '(%s+)do',
	function(self, StrObject, PositionObject, IsValidPos)
		local PList = TScope.ResolvePositions(StrObject, PositionObject);
		local ClassBody = PList.Body;
		local CPIN = PositionObject.positionStart;
		local _, _, Spaces1, ClassName, Spaces2, Spaces3, Spaces4, ParentName, Spaces5 = PList.InitialString:find(self.Mask); 
		local ImplementationExists = false;
		local Scopes = {};
		local KeyWords = TList.New {'public', 'private', 'protected'};
		function KeyWords:In(value)
			for i = 1, #self do 
				if self[i] == value then 
					return true, i;
				end;
			end;
			return false, nil;
		end;
		local StartPos = 0;
		local ScopeName = 'Public';
		local PS, PF, Word = ClassBody:find('([%w]+)');
		while (PS ~= nil) do 
			-- To allow strings and comments.
			if (IsValidPos(CPIN + PS - 1) and IsValidPos(CPIN + PF - 1)) then
				if KeyWords:In(Word) then 
					table.insert(Scopes, ScopeName + ' { ' + (ClassBody:sub(StartPos, ((PS - 1) > 0) and (PS - 1) or 0) or '') + ' }; ');
					StartPos = PF + 1;
					ScopeName = (Word:sub(1, 1):upper()) + Word:sub(2);
				end;
				if (Word == 'implementation') then 
					ImplementationExists = true;
					break;
				end;
			end;
			PS, PF, Word = ClassBody:find('([%w]+)', PF + 1);
		end;
		table.insert(Scopes, ScopeName + ' { ' + (ClassBody:sub(StartPos, (ImplementationExists and (((PS - 1) > 0) and (PS - 1) or 0) or nil)) or '') + ' }; ');
		local InternalImplementation = '';
		if ImplementationExists then 
			InternalImplementation = '["Implementation"] = function()' + (ClassBody:sub(PF + 1) or '') + 'end;';
		end;
		local Resulting = Spaces1 + 
						  Spaces2 + 
						  Spaces3 + 
						  Spaces4 + 
						  Spaces5 + 
						  'CreateClass({ ' + table.concat(Scopes) + InternalImplementation + '}, "' + ClassName + '", "' + ParentName +'")';
		return Resulting;
	end 
);

local ClassScopeNotInherited = TScope.New(
	'class',
	'class(%s+)' + VarNameMask + '(%s+)do',
	function(self, StrObject, PositionObject, IsValidPos)
		local PList = TScope.ResolvePositions(StrObject, PositionObject);
		local ClassBody = PList.Body;
		local CPIN = PositionObject.positionStart;
		local _, _, Spaces1, ClassName, Spaces2 = PList.InitialString:find(self.Mask); 
		local ImplementationExists = false;
		local Scopes = {};
		local KeyWords = TList.New {'public', 'private', 'protected'};
		function KeyWords:In(value)
			for i = 1, #self do 
				if self[i] == value then 
					return true, i;
				end;
			end;
			return false, nil;
		end;
		local StartPos = 0;
		local ScopeName = 'Public';
		local PS, PF, Word = ClassBody:find('([%w]+)');
		while (PS ~= nil) do 
			if (IsValidPos(CPIN + PS - 1) and IsValidPos(CPIN + PF - 1)) then 
				if KeyWords:In(Word) then 
					table.insert(Scopes, ScopeName + ' { ' + (ClassBody:sub(StartPos, ((PS - 1) > 0) and (PS - 1) or 0) or '') + ' }; ');
					StartPos = PF + 1;
					ScopeName = (Word:sub(1, 1):upper()) + Word:sub(2);
				end;
				if (Word == 'implementation') then 
					ImplementationExists = true;
					break;
				end;
			end;
			PS, PF, Word = ClassBody:find('([%w]+)', PF + 1);
		end;
		table.insert(Scopes, ScopeName + ' { ' + (ClassBody:sub(StartPos, (ImplementationExists and (((PS - 1) > 0) and (PS - 1) or 0) or nil)) or '') + ' }; ');
		local InternalImplementation = '';
		if ImplementationExists then 
			InternalImplementation = '["Implementation"] = function()' + (ClassBody:sub(PF + 1) or '') + 'end;';
		end;
		local Resulting = Spaces1 + Spaces2 + 'CreateClass({ ' + table.concat(Scopes) + InternalImplementation + '}, "' + ClassName + '")';
		return Resulting;
	end 
);

HELLua.ScopesList = TList.New {
	ExceptScope,
	FinallyScope,
	StriclyWithScope,
	WithScope,
	ForLoopC_Scope,
	ClassScopeInherited;
	ClassScopeNotInherited;
};

-- Macroses

local Macro_CScopeClose = TMacros.New(
	'([^%]])}',
	function(self, str, pattern)
		return str:gsub(pattern, '%1 end ');
	end
);

local Macro_CElseScope = TMacros.New(
	'}%s-else%s-{',
	function(self, str, pattern, __, start)
		print(start);
		print(__);
		return start + 2, ' else ';
	end
);

local Macro_CElseIfScope = TMacros.New(
	'}%s-elseif(.-){',
	function(self, str, pattern, __, start)
		return str:gsub(pattern, ' elseif %1 then '), nil, start + 2;
	end
);

local Macro_CIfScope = TMacros.New(
	'%sif(.-){',
	function(self, str, pattern, __, start)
		return str:gsub(pattern, ' if %1 then '), nil, start + 2;
	end
);

local Macro_CFunctionScope = TMacros.New(
	'function(%s+[%a_].-){',
	function(self, str, pattern, __, start, finish)
		return str:gsub(pattern, 'function %1'), nil, start + 1;
	end
);

local Macro_CFunctionScopeAnonymous = TMacros.New(
	'function(%s-%(.-){',
	function(self, str, pattern, __, start, finish)
		return str:gsub(pattern, 'function %1'), nil, start + 1;
	end
);

local Macro_CScopeOpen = TMacros.New(
	'{[^%[]',
	function(self, str, pattern)
		return ' do ';
	end
);

local Macro_CArrayOpen = TMacros.New(
	'{%[',
	function() return ' { '; end
);

local Macro_CArrayClose = TMacros.New(
	'%]}',
	function() return ' } '; end
);

HELLua.MacrosesCSyntax = TList.New {
	Macro_CElseScope;
	Macro_CElseIfScope;
	Macro_CIfScope;
	Macro_CFunctionScope;
	Macro_CFunctionScopeAnonymous;
	Macro_CScopeOpen;
	Macro_CScopeClose;
	Macro_CArrayOpen;
	Macro_CArrayClose;
};

local Macro_PascalAssign = TMacros.New(
	'([^%p])%:%=([^%p])',
	function(self, str, pattern)
		return str:gsub(pattern, '%1=%2')
	end
)

local Macro_PascalEquals = TMacros.New(
	'([^%p])%=([^%p])',
	function(self, str, pattern)
		return str:gsub(pattern, '%1==%2')
	end
)

HELLua.MacrosesPascal = TList.New {
	Macro_PascalEquals;
	Macro_PascalAssign;
};

local Macro_AndBoolC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s-&&%%s-%s', 2),
	function(self, str, pattern)
		-- return str:gsub(pattern, '%1 and %2')
		local _, _, Op1, Op2 = str:find(pattern);
		if ((Op1 == 'false') or (Op2 == 'false')) and Machine.AllowSimpleCodeOptimization then
			if Machine.AllowHints then
				Machine:Msg(Compiling_Hint_Optimization, '&&', 'false');
			end;
			return 'false';
		elseif (Op1 == 'true') and (Op2 == 'true') and Machine.AllowSimpleCodeOptimization then
			if Machine.AllowHints then
				Machine:Msg(Compiling_Hint_Optimization, '&&', 'true');
			end;
			return 'true';
		else
			return ('%s and %s'):format(Op1, Op2);
		end;
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "&&" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]&&[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_AndBitC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s-&%%s-%s', 2),
	function(self, str, pattern)
		--return str:gsub(pattern, 'bit%.band%( %1, %2 %)')
		local _, _, Op1, Op2 = str:find(pattern);
		if ((Op1 == '0') or (Op2 == '0')) and Machine.AllowSimpleCodeOptimization then
			if Machine.AllowHints then
				Machine:Msg(Compiling_Hint_Optimization, '&', '0');
			end;
			return '0';
		else
			return ('(bit.band(%s, %s))'):format(Op1, Op2);
		end;
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "&" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]&[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_OrBoolC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s-%%|%%|%%s-%s', 2),
	function(self, str, pattern)
		-- return str:gsub(pattern, '%1 or %2')
		local _, _, Op1, Op2 = str:find(pattern);
		if ((Op1 == 'true') or (Op2 == 'true')) and Machine.AllowSimpleCodeOptimization then
			if Machine.AllowHints then
				Machine:Msg(Compiling_Hint_Optimization, '||', 'true');
			end;
			return 'true';
		elseif (Op1 == 'false') and (Op2 == 'false') and Machine.AllowSimpleCodeOptimization then
			if Machine.AllowHints then
				Machine:Msg(Compiling_Hint_Optimization, '||', 'false');
			end;
			return 'false';
		else
			return ('%s or %s'):format(Op1, Op2);
		end;
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "||" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]||[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_OrBitC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s-%%|%%s-%s', 2),
	function(self, str, pattern)
		-- return str:gsub(pattern, 'bit%.bor%( %1, %2 %)')
		local _, _, Op1, Op2 = str:find(pattern);
		if (Op1 == '0') and (Op2 == '0') and Machine.AllowSimpleCodeOptimization then
			if Machine.AllowHints then
				Machine:Msg(Compiling_Hint_Optimization, '|', '0');
			end;
			return '0';
		elseif ((Op1 == '0') or (Op2 == '0')) and Machine.AllowSimpleCodeOptimization then
			if (Op1 == '0') then
				if Machine.AllowHints then
					Machine:Msg(Compiling_Hint_Optimization, '|', Op2);
				end;
				return Op2;
			else
				if Machine.AllowHints then
					Machine:Msg(Compiling_Hint_Optimization, '|', Op1);
				end;
				return Op1;
			end;
		else
			return ('(bit.bor(%s, %s))'):format(Op1, Op2);
		end;
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "|" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]|[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_ShlBitC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s-<<%%s-%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.lshift(%s, %s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "<<" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]<<[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_ShrBitC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s->>%%s-%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.rshift(%s, %s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: ">>" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]>>[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_ShlBitPascal = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+shl%%s+%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.lshift(%s, %s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "shl" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%sshl%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_ShrBitPascal = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+shr%%s+%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.rshift(%s, %s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "shr" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%sshr%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_XorBoolPascal = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+xor%%s+%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('((not %s and %s) or (%s and not %s))'):format(Op1, Op2, Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "xor" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%sxor%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_XorBitPascal = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+bxor%%s+%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.bxor(%s, %s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "bxor" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%sbxor%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_NotBitPascal = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '([^%%w])bnot%%s+%s', 1),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('%s(bit.bnot(%s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "bnot" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%sbnot%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_IntNotBitPascal = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+snot%%s+%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.band(bit.bnot(%s), bit.lshift(1, %s) - 1))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "snot" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%ssnot%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_AndBitPascal = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+band%%s+%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.band(%s, %s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "band" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%sband%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_OrBitPascal = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+bor%%s+%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.bor(%s, %s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "bor" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%sbor%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_XorBitC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s-%%~%%^%%s-%s', 2),
	function(self, str, pattern)
		local _, _, Op1, Op2 = str:find(pattern);
		return ('(bit.bxor(%s, %s))'):format(Op1, Op2);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "~^" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '%s%~%^%s';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_NotBoolC = TMacros.New(
	'!([^=])',
	function(self, str, pattern)
		return str:gsub(pattern, ' not %1');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "!" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]![^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_NotBitC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '~%%s-%s', 1),
	function(self, str, pattern)
		return str:gsub(pattern, '(bit%.bnot%(%1%))');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "~" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]~[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_NotEqualC = TMacros.New(
	'([^%p])%!%=([^%p])',
	function(self, str, pattern)
		return str:gsub(pattern, '%1~=%2');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "!=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]!=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

HELLua.MacrosesCLogic = TList.New {
	Macro_NotBoolC;
	Macro_NotBitC;
	Macro_NotBitPascal;
	Macro_IntNotBitPascal;
	Macro_NotEqualC;
	--[[Macro_AndBoolC =]] Macro_AndBoolC;
	--[[Macro_AndBitC  =]] Macro_AndBitC;
	Macro_AndBitPascal;
	--[[Macro_OrBoolC  =]] Macro_OrBoolC;
	--[[Macro_OrBitC   =]] Macro_OrBitC;
	Macro_OrBitPascal;
	Macro_XorBoolPascal;
	Macro_XorBitC;
	Macro_XorBitPascal;
	Macro_ShlBitC;
	Macro_ShlBitPascal;
	Macro_ShrBitC;
	Macro_ShrBitPascal;
};

local Macro_PlusEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%+=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = %1 + %2');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "+=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]%+%=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_MinusEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%-=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = %1 - %2');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "-=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]%-%=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_MultiplyEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%*=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		-- return str:gsub(pattern, '%1 = %1 * %2');
		local _, _, Op1, Op2 = str:find(pattern);
		if (Op2 == '0') and Machine.AllowSimpleCodeOptimization then
			if Machine.AllowHints then
				Machine:Msg(Compiling_Hint_Optimization, '*=', '0');
			end;
			return ('%s = 0'):format(Op1);
		elseif (Op2 == '1') and Machine.AllowSimpleCodeOptimization then
			if Machine.AllowHints then
				Machine:Msg(Compiling_Hint_Optimization, '*=', Op1);
			end;
			return ('%s = %s'):format(Op1, Op1);
		else
			return ('%s = %s * %s'):format(Op1, Op1, Op2);
		end;
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "*=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]%*=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_DivideEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%/=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = %1 / %2');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "/=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]/=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_AndEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%&=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = bit%.band%( %1 , %2 %)');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "&=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]&=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_OrEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%|=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = bit%.bor%( %1 , %2 %)');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "|=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]|=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_XorEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%^=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = bit%.bxor%( %1 , %2 %)');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "^=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]%^=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_ShlEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%<<=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = bit%.lshift%( %1 , %2 %)');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "<<=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]%<%<=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_ShrEqual = TMacros.New(
	(function()
		local Arr = HELLua.IterateAll(ValueOrFunction, '%%>>=%%s-%s', 1);
		Arr = (VarMask + '%s-'):PreConcat(Arr);
		return Arr;
	end)(),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = bit%.rshift%( %1 , %2 %)');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: ">>=" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]%>%>=[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_PlusPlusPost = TMacros.New(
	'%+%+%s-' + VarMask,
	function(self, str, pattern)
		return str:gsub(pattern, '(function() %1 = %1 + 1; return %1; end)()')
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "++" operator is not compiled at [[%d]]. \n';
		local Msk = '[^%p]%+%+[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_PlusPlusPre = TMacros.New(
	VarMask + '%s-%+%+',
	function(self, str, pattern)
		local _, _, Name = str:find(pattern);
		local LocalName = 'A';
		while LocalName == Name do 
			LocalName = LocalName + 'A';
		end;
		return str:gsub(pattern, '(function() local ' + LocalName + ' = %1; %1 = %1 + 1; return ' + LocalName + '; end)()')
	end
);

local Macro_DivPascal = TMacros.New(
	ValueMask + '%s+div%s+' + ValueMask,
	function(self, str, pattern)
		return str:gsub(pattern, ' (math.floor((%1) / (%2))) ');
	end
);

local Macro_ModPascal = TMacros.New(
	ValueMask + '%s+mod%s+' + ValueMask,
	function(self, str, pattern)
		return str:gsub(pattern, '(%1) %% (%2)');
	end
);

local Macro_DivC = TMacros.New(
	ValueMask + '%s-//%s-' + ValueMask,
	function(self, str, pattern)
		return str:gsub(pattern, '(math.floor((%1) / (%2)))');
	end
);

local Macro_DivEqual = TMacros.New(
	VarMask:PreConcat(HELLua.IterateAll(ValueOrFunction, '%%s-//=%%s-%s', 1)),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = (math.floor((%1) / (%2)))');
	end
);

local Macro_ModEqual = TMacros.New(
	VarMask:PreConcat(HELLua.IterateAll(ValueOrFunction, '%%s-%%%%=%%s-%s', 1)),
	function(self, str, pattern)
		return str:gsub(pattern, '%1 = (%1) %% (%2)');
	end
);

HELLua.MacrosesCMath = TList.New {
	Macro_PlusEqual;
	Macro_DivEqual;
	Macro_ModEqual;
	Macro_DivPascal;
	Macro_DivC;
	Macro_ModPascal;
	Macro_MinusEqual;
	Macro_MultiplyEqual;
	Macro_DivideEqual;
	Macro_AndEqual;
	Macro_OrEqual;
	Macro_XorEqual;
	Macro_ShlEqual;
	Macro_ShrEqual;
	Macro_PlusPlusPost;
	Macro_PlusPlusPre;
};

local Macro_Ternary = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s-%%?%%s-%s%%s-:%%s-%s', 3),
	function(self, str, pattern)
		return str:gsub(pattern, '%(%1 and %2 or %3%)');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "?" operator is not compiled at [[%d]] - ' + Check_VarsMsg;
		local Msk = '[^%p]%?[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

HELLua.MacrosesCTernary = TList.New {
	Macro_Ternary;
};

local Macro_EnhancedTypeCheck = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s-%%:%%?%%s-(%%w+)', 1),
	function(self, str, pattern)
		local PS, PF, F, S = str:find(pattern);
		S = S:lower();
		return str:gsub(pattern, '%(type%(%1%) %=%= "') + S + '")';
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: ":?" operator is not compiled at [[%d]] - (var, number, brackets) :? (string). \n';
		local Msk = '[^%p]%:%?[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_EnhancedTypeOf = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '([^%%w])typeof%%s+%s', 1),
	function(self, str, pattern)
		-- local PS, PF, F, S = str:find(pattern);
		return str:gsub(pattern, '%1type(%2)');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: ":?" operator is not compiled at [[%d]] - (var, number, brackets) :? (string). \n';
		local Msk = '[^%p]%:%?[^%p]';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_EnhancedTypeCast = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+to%%s+(%%w+)', 1),
	function(self, str, pattern)
		return str:gsub(pattern, '%(to%2%(%1%)%)');
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "to" operator is not compiled at [[%d]] - (var, number, brackets) to (string). \n';
		local Msk = '%s+[^%p]to[^%p]%s+';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_EnhancedRaiseError = TMacros.New(
	'([%s;])raise%s+(.-);',
	function(self, str, pattern)
		return str:gsub(pattern, '%1error(%2);');
	end,
	function()
	end
);

local Macro_EnhancedThrowError = TMacros.New(
	'([%s;])throw%s+(.-);',
	function(self, str, pattern)
		return str:gsub(pattern, '%1error(%2);');
	end,
	function()
	end
);

local Macro_EnhancedNull = TMacros.New(
	'([^%w])null([^%w])',
	function(self, str, pattern)
		return str:gsub(pattern, '%1nil%2');
	end,
	function()
	end
);

local Macro_PascalForTo = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '(%%s)for%%s+' + VarMask:gsub('%%', '%%%%') + '%%s-=%%s-%s%%s+to%%s+%s%%s+do', 2),
	function(self, str, pattern)
		return str:gsub(pattern, '%1for %2 = %3, %4 do');
	end,
	function()
	end
);

local Macro_PascalForDownTo = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '(%%s)for%%s+' + VarMask:gsub('%%', '%%%%') + '%%s-=%%s-%s%%s+downto%%s+%s%%s+do', 2),
	function(self, str, pattern)
		return str:gsub(pattern, '%1for %2 = %3, %4, -1 do');
	end,
	function()
	end
);

local Macro_EnhancedNewConstructor = TMacros.New(
	'([^%w])new%s+(.-)(%b())',
	function(self, str, pattern)
		return str:gsub(pattern, '%1%2.New%3');
	end,
	function()
	end
);

local Macro_EnhancedLoader = TMacros.New(
	--('uses(%s+[%w%_%.]+(%s+as%s+[%w%_%.]+)-,)-(%s+[%w%_%.]+(%s+as%s+[%w%_%.]+)%s+);'),
	('([%s;])uses%s+([%w%_%s%.%,]+);'),
	function(self, str, pattern)
		local _, _, Symbol = str:find('([%s;])uses');
		local NormalPatternWithUses = 'uses%s(%s-)([%w%_%.]+)%s-[,;]';
		local NormalPattern = ',(%s-)([%w%_%.]+)%s-[,;]';
		local NamedPattern =  '(%s-)([%w%_%.]+)%s+as%s+([%w%_%.]+)%s-[,;]';
		local UsesStr = Symbol + '--[[ Uses clause ]] ';

		-- Normal Pattern With `Uses`
		local PS, PF, Spaces, Name = str:find(NormalPatternWithUses);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%slocal %s = require \'%s\';'):format(Spaces, Name, Name);
			PS, PF, Spaces, Name = str:find(NormalPatternWithUses, PF + 1);
		end;

		-- Normal Pattern
		local PS, PF, Spaces, Name = str:find(NormalPattern);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%slocal %s = require \'%s\';'):format(Spaces, Name, Name);
			PS, PF, Spaces, Name = str:find(NormalPattern, PF + 1);
		end;

		-- Named Pattern
		local PS, PF, Spaces, Name, VariableName = str:find(NamedPattern);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%slocal %s = require \'%s\';'):format(Spaces, VariableName, Name);
			PS, PF, Spaces, Name, VariableName = str:find(NamedPattern, PF + 1);
		end;

		-- UsesStr = UsesStr + '\n';
		return UsesStr;
	end,
	function()
	end
);

local Macro_EnhancedDoFileLoader = TMacros.New(
	('([%s;])assemble%s+([%w%_%s%.%,]+);'),
	function(self, str, pattern)
		local _, _, Symbol = str:find('([%s;])assemble');
		local NormalPatternWithUses = 'assemble%s(%s-)([%w%_%.]+)%s-[,;]';
		local NormalPattern = ',(%s-)([%w%_%.]+)%s-[,;]';
		local NamedPattern =  '(%s-)([%w%_%.]+)%s+as%s+([%w%_%.]+)%s-[,;]';
		local UsesStr = Symbol + '--[[ Uses clause ]] ';

		-- Normal Pattern With `Uses`
		local PS, PF, Spaces, Name = str:find(NormalPatternWithUses);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%slocal %s = dofile \'%s\';'):format(Spaces, Name, Name);
			PS, PF, Spaces, Name = str:find(NormalPatternWithUses, PF + 1);
		end;

		-- Normal Pattern
		local PS, PF, Spaces, Name = str:find(NormalPattern);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%slocal %s = dofile \'%s\';'):format(Spaces, Name, Name);
			PS, PF, Spaces, Name = str:find(NormalPattern, PF + 1);
		end;

		-- Named Pattern
		local PS, PF, Spaces, Name, VariableName = str:find(NamedPattern);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%slocal %s = dofile \'%s\';'):format(Spaces, VariableName, Name);
			PS, PF, Spaces, Name, VariableName = str:find(NamedPattern, PF + 1);
		end;

		-- UsesStr = UsesStr + '\n';
		return UsesStr;
	end,
	function()
	end
);

local Macro_EnhancedGlobalLoader = TMacros.New(
	--('uses(%s+[%w%_%.]+(%s+as%s+[%w%_%.]+)-,)-(%s+[%w%_%.]+(%s+as%s+[%w%_%.]+)%s+);'),
	('([%s;])global%s+uses%s+([%w%_%s%.%,]+);'),
	function(self, str, pattern)
		local _, _, Symbol = str:find('([%s;])global%s+uses');
		local NormalPatternWithUses = 'uses%s(%s-)([%w%_%.]+)%s-[,;]';
		local NormalPattern = ',(%s-)([%w%_%.]+)%s-[,;]';
		local NamedPattern =  '(%s-)([%w%_%.]+)%s+as%s+([%w%_%.]+)%s-[,;]';
		local UsesStr = Symbol + '--[[ Uses clause ]] ';

		-- Normal Pattern With `Uses`
		local PS, PF, Spaces, Name = str:find(NormalPatternWithUses);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%s%s = require \'%s\';'):format(Spaces, Name, Name);
			PS, PF, Spaces, Name = str:find(NormalPatternWithUses, PF + 1);
		end;

		-- Normal Pattern
		local PS, PF, Spaces, Name = str:find(NormalPattern);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%s%s = require \'%s\';'):format(Spaces, Name, Name);
			PS, PF, Spaces, Name = str:find(NormalPattern, PF + 1);
		end;

		-- Named Pattern
		local PS, PF, Spaces, Name, VariableName = str:find(NamedPattern);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%s%s = require \'%s\';'):format(Spaces, VariableName, Name);
			PS, PF, Spaces, Name, VariableName = str:find(NamedPattern, PF + 1);
		end;

		-- UsesStr = UsesStr + '\n';
		return UsesStr;
	end,
	function()
	end
);

local Macro_EnhancedGlobalDoFileLoader = TMacros.New(
	('([%s;])global%s+assemble%s+([%w%_%s%.%,]+);'),
	function(self, str, pattern)
		local _, _, Symbol = str:find('([%s;])global%s+assemble');
		local NormalPatternWithUses = 'assemble%s(%s-)([%w%_%.]+)%s-[,;]';
		local NormalPattern = ',(%s-)([%w%_%.]+)%s-[,;]';
		local NamedPattern =  '(%s-)([%w%_%.]+)%s+as%s+([%w%_%.]+)%s-[,;]';
		local UsesStr = Symbol + '--[[ assemble clause ]] ';

		-- Normal Pattern With `Uses`
		local PS, PF, Spaces, Name = str:find(NormalPatternWithUses);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%s%s = dofile \'%s\';'):format(Spaces, Name, Name);
			PS, PF, Spaces, Name = str:find(NormalPatternWithUses, PF + 1);
		end;

		-- Normal Pattern
		local PS, PF, Spaces, Name = str:find(NormalPattern);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%s%s = dofile \'%s\';'):format(Spaces, Name, Name);
			PS, PF, Spaces, Name = str:find(NormalPattern, PF + 1);
		end;

		-- Named Pattern
		local PS, PF, Spaces, Name, VariableName = str:find(NamedPattern);
		while not (PS == nil) do
			UsesStr = UsesStr + ('%s%s = dofile \'%s\';'):format(Spaces, VariableName, Name);
			PS, PF, Spaces, Name, VariableName = str:find(NamedPattern, PF + 1);
		end;

		-- UsesStr = UsesStr + '\n';
		return UsesStr;
	end,
	function()
	end
);

local Macro_EnhancedInheritsFrom = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+is%%s+instance%%s+of%%s+(%%w+)', 1),
	function(self, str, pattern)
		return str:gsub(pattern, '%1.InheritsFrom("%2")');
	end
);

local Macro_EnhancedTypeCheckTypedNotC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+is%%s+(%%w+)%%s-(%%b<>)', 1),
	function(self, str, pattern)
		local PS, PF, F, S, T = str:find(pattern);
		S = S:lower();
		T = T:sub(2, -2);
		return str:gsub(pattern, '%(type%(%1%) %=%= "' + S + '") and (%1.InheritsFrom("' + T + '"))');
	end
);

local Macro_EnhancedTypeCheckNotC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+is%%s+(%%w+)', 1),
	function(self, str, pattern)
		local PS, PF, F, S = str:find(pattern);
		S = S:lower();
		return str:gsub(pattern, '%(type%(%1%) %=%= "') + S + '")';
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "is" operator is not compiled at [[%d]] - (var, number, brackets) is (string). \n';
		local Msk = '%s+is%s+';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_EnhancedTypeCheckRawNotC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+raw%%s+is%%s+(%%w+)', 1),
	function(self, str, pattern)
		local PS, PF, F, S = str:find(pattern);
		S = S:lower();
		return str:gsub(pattern, '%(RawType%(%1%) %=%= "') + S + '")';
	end
);

local Macro_EnhancedTypeCheckInverseNotC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+is%%s+not%%s+(%%w+)', 1),
	function(self, str, pattern)
		local PS, PF, F, S = str:find(pattern);
		S = S:lower();
		return str:gsub(pattern, 'not %(type%(%1%) %=%= "') + S + '")';
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "is not" operator is not compiled at [[%d]] - (var, number, brackets) isnot (string). \n';
		local Msk = '%s+is%s+not%s+';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_EnhancedTypeCheckInverseRawNotC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+raw%%s+is%%s+not%%s+(%%w+)', 1),
	function(self, str, pattern)
		local PS, PF, F, S = str:find(pattern);
		S = S:lower();
		return str:gsub(pattern, 'not %(RawType%(%1%) %=%= "') + S + '")';
	end
);

local Macro_EnhancedTypeCastAnother = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+as%%s+(%%w+)', 1),
	function(self, str, pattern)
		local PS, PF, F, S = str:find(pattern);
		S = S:lower();
		-- return str:gsub(pattern, '%(to%2%(%1%)%)');
		return ('to%s(%s)'):format(S, F);
	end,
	function(Str, Warn, IsValidPos, Machine)
		local Msg = 'Syntax Error: "as" operator is not compiled at [[%d]] - (var, number, brackets) as (string)';
		local Msk = '%s+as%s+';
		TLua.ParseStringForPatterns(Str, Msk, function(Start, Finish)
			if not (IsValidPos(Start)) or not (IsValidPos(Finish)) then
				return nil, nil;
			end;
			if Warn then
				if Machine.AllowWarnings then
					Machine:Msg('[Compiling::Check][Warning]: ' + Msg, Start);
				end;
			else
				Machine:Msg('[Compiling::Check][Error]: ' + Msg, Start);
				RunTimeError('Error during check has been occured. Building is stopped.');
			end;
		end);
	end
);

local Macro_EnhancedImport_Double = TMacros.New(
	VarMask:PostConcat(HELLua.IterateAll(ValueOrFunction, 'import%%s+(".-")%%s+from%%s+%s%%s+to%%s+', 1)),
	function(self, str, pattern)
		return str:gsub(pattern, 'Import(%1, %2, %3)');
	end
); 

local Macro_EnhancedImport = TMacros.New(
	VarMask:PostConcat(HELLua.IterateAll(ValueOrFunction, 'import%%s+(\'.-\')%%s+from%%s+%s%%s+to%%s+', 1)),
	function(self, str, pattern)
		return str:gsub(pattern, 'Import(%1, %2, %3)');
	end
);

local Macro_EnhancedTypeAssignNotC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+is%%s+(%%w+)%%s-otherwise%%s-%s', 2),
	function(self, str, pattern)
		local Type = str:gsub(pattern, '%2'):lower();
		return str:gsub(pattern, '((type(%1) == "' + Type + '") and (%1) or (%3))');
	end
);

local Macro_EnhancedTypeAssignSelfedNotC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '([^%%w])self%%s+%s%%s+is%%s+(%%w+)%%s-otherwise%%s-%s', 2),
	function(self, str, pattern)
		local Type = str:gsub(pattern, '%3'):lower();
		return str:gsub(pattern, '%1 %2 = ((type(%2) == "' + Type + '") and (%2) or (%4))');
	end
);

local Macro_EnhancedTypeAssignC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '%s%%s+:%%?%%s+(%%w+)%%s-%%->%%s-%s', 2),
	function(self, str, pattern)
		local Type = str:gsub(pattern, '%2'):lower();
		return str:gsub(pattern, '((type(%1) == "' + Type + '") and (%1) or (%3))');
	end
);

local Macro_EnhancedTypeAssignSelfedC = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '([^%%w])self%%s+%s%%s+:%%?%%s+(%%w+)%%s-%%->%%s-%s', 2),
	function(self, str, pattern)
		local Type = str:gsub(pattern, '%3'):lower();
		return str:gsub(pattern, '%1 %2 = ((type(%2) == "' + Type + '") and (%2) or (%4))');
	end
);

local Macro_EnhancedRecord = TMacros.New(
	'record%s+from%s+' + VarMask,
	function(self, str, pattern)
		return str:gsub(pattern, 'CreateRecord(%1)');
	end
);

local Macro_EnhancedRecordInterited = TMacros.New(
	'record%s+from%s+' + VarMask + '%s+inherits%s+from%s+' + VarMask,
	function(self, str, pattern)
		return str:gsub(pattern, 'CreateRecordInherited(%1, %2)');
	end
);

local Macro_EnhancedUseless = TMacros.New(
	HELLua.IterateAll({ QuotientMask, ValueMask }, '%s%%s+of%%s+no%%s+use([^%%w])', 1),
	function(self, str, pattern)
		return str:gsub(pattern, 'local _ = %1 %2');
	end
);

--[[
	Added support to `is` and `as` operators.

	local A = 12;
	if A is number then
		return A as string;
	end;
]]

HELLua.MacrosesEnhanced = TList.New {
	Macro_EnhancedNull;
	Macro_EnhancedTypeAssignSelfedNotC;
	Macro_EnhancedTypeAssignSelfedC;
	Macro_EnhancedTypeAssignNotC;
	Macro_EnhancedTypeAssignC;
	-- Macro_EnhancedRecordInterited;
	-- Macro_EnhancedRecord;
	Macro_EnhancedGlobalLoader;
	Macro_EnhancedGlobalDoFileLoader;
	Macro_EnhancedLoader;
	Macro_EnhancedDoFileLoader;
	Macro_EnhancedImport;
	Macro_EnhancedImport_Double;
	Macro_PascalForTo;
	Macro_PascalForDownTo;
	Macro_EnhancedRaiseError;
	Macro_EnhancedThrowError;
	Macro_EnhancedNewConstructor;
	Macro_EnhancedTypeOf;
	Macro_EnhancedInheritsFrom;
	Macro_EnhancedTypeCheckInverseRawNotC;
	Macro_EnhancedTypeCheckRawNotC;
	Macro_EnhancedTypeCheckTypedNotC;
	Macro_EnhancedTypeCheckInverseNotC;
	Macro_EnhancedTypeCheck;
	Macro_EnhancedTypeCast;
	Macro_EnhancedTypeCheckNotC;
	Macro_EnhancedTypeCastAnother;
	Macro_EnhancedUseless;
};

local Macro_Class_Read = TMacros.New(
	'([^%w])read%s+' + VarNameMask,
	function(self, str, pattern)
		return str:gsub(pattern, '%1, ["Read"] = "%2"');
	end
);

local Macro_Class_Write = TMacros.New(
	'([^%w])write%s+' + VarNameMask,
	function(self, str, pattern)
		return str:gsub(pattern, '%1, ["Write"] = "%2"');
	end
);

local Macro_Class_Default = TMacros.New(
	HELLua.IterateAll(ValueOrFunction, '([%%s,]+)default%%s+%s', 1),
	function(self, str, pattern)
		return str:gsub(pattern, ' , ["Content"] = %2');
	end
);

local Macro_Class_Static = TMacros.New(
	'([%s,]+)static([^%w])',
	function(self, str, pattern)
		return str:gsub(pattern, ' , ["Static"] = true%2');
	end
);

local Macro_Class_Enclosed = TMacros.New(
	'([%s,]+)enclosed([^%w])',
	function(self, str, pattern)
		return str:gsub(pattern, ' , ["Enclosed"] = true%2');
	end
);

local Macro_Class_Constant = TMacros.New(
	'([%s,]+)constant([^%w])',
	function(self, str, pattern)
		return str:gsub(pattern, ' , ["Constant"] = true%2');
	end
);

local Macro_Class_Abstract = TMacros.New(
	'([%s,]+)abstract([^%w])',
	function(self, str, pattern)
		return str:gsub(pattern, ' , ["Abstract"] = true%2');
	end
);

local Macro_Class_Virtual = TMacros.New(
	'([%s,]+)virtual([^%w])',
	function(self, str, pattern)
		return str:gsub(pattern, ' , ["Virtual"] = true%2');
	end
);

local Macro_Class_Operator = TMacros.New(
	'([%s,]+)operator([^%w])',
	function(self, str, pattern)
		return str:gsub(pattern, ' , ["Operator"] = true%2');
	end
);

local Macro_Class_LinkedWith = TMacros.New(
	'([%s,]+)linked%s+with%s+' + VarNameMask,
	function(self, str, pattern)
		return str:gsub(pattern, ' , ["Linked"] = "%2"');
	end
);

local Macro_Class_Constructor = TMacros.New(
	'([^%w])constructor(%s+)' + VarNameMask + '(%s-);',
	function(self, str, pattern)
		return str:gsub(pattern, ' %1Method%2{ ["Name"] = "%3", ["Constructor"] = true };');
	end
);

local Macro_Class_FieldRaw = TMacros.New(
	'([^%w])field(%s+)' + VarNameMask + '(%s-);',
	function(self, str, pattern)
		return str:gsub(pattern, ' %1Field%2{ ["Name"] = "%3" };');
	end
);

local Macro_Class_MethodRaw = TMacros.New(
	'([^%w])method(%s+)' + VarNameMask + '(%s-);',
	function(self, str, pattern)
		return str:gsub(pattern, ' %1Method%2{ ["Name"] = "%3" };');
	end
);

local Macro_Class_ConstructorModified = TMacros.New(
	'([^%w])constructor(%s+)' + VarNameMask + '(%s+)(.-);',
	function(self, str, pattern)
		return str:gsub(pattern, ' %1Method%2{ ["Name"] = "%3", ["Constructor"] = true %4%5 };');
	end
);

local Macro_Class_FieldModified = TMacros.New(
	'([^%w])field(%s+)' + VarNameMask + '([%s,]+)(.-);',
	function(self, str, pattern)
		return str:gsub(pattern, ' %1Field%2{ ["Name"] = "%3" %4%5 };');
	end
);

local Macro_Class_MethodModified = TMacros.New(
	'([^%w])method(%s+)' + VarNameMask + '([%s,]+)(.-);',
	function(self, str, pattern)
		return str:gsub(pattern, ' %1Method%2{ ["Name"] = "%3" %4%5 };');
	end
);

local Macro_Class_PropertyModified = TMacros.New(
	'([^%w])property(%s+)' + VarNameMask + '(%s+)(.-);',
	function(self, str, pattern)
		return str:gsub(pattern, ' %1Property%2{ ["Name"] = "%3" %4%5 };');
	end
);

local Macro_Class_Inherited = TMacros.New(
	'([^%w])inherited%s+' + VarNameMask,
	function(self, str, pattern)
		return str:gsub(pattern, ' %1Inherited("%2")');
	end
);

HELLua.MacrosesClass = TList.New {
	Macro_Class_Read;
	Macro_Class_Write;
	Macro_Class_Default;
	Macro_Class_Static;
	Macro_Class_Enclosed;
	Macro_Class_Constant;
	Macro_Class_Abstract;
	Macro_Class_Virtual;
	Macro_Class_Operator;
	Macro_Class_LinkedWith;
	Macro_Class_Constructor;
	Macro_Class_FieldRaw;
	Macro_Class_MethodRaw;
	Macro_Class_ConstructorModified;
	Macro_Class_FieldModified;
	Macro_Class_MethodModified;
	Macro_Class_PropertyModified;
	Macro_Class_Inherited;
};

local Macro_Try = TMacros.New(
	'([%s;])try(%s)',
	function(self, str, pattern, GrandStr, Start, Finish)
		local _, _, Symbol, SpaceAfter = str:find('([%s;])try(%s)');
		local PS, PF = TLua.ScanScope(GrandStr, Finish + 1, '%w+', {{Open = 'try'; Close = {'except'; 'finally'}}}, 'try');
		-- Log('PS = %s, PF = %s', PS, PF);
		local FS, FF = (GrandStr):find('except%s+on%s+' + VarMask + '%s+do', PS);
		-- Log('FS = %s, FF = %s', tostring(FS), tostring(FF));
		if not ((FS ~= nil) and (FS == PS)) then
			FS, FF = (GrandStr):find('finally%s+do', PS);
			-- Log('FS = %s, FF = %s', tostring(FS), tostring(FF));
			if not ((FS ~= nil) and (FS == PS)) then
				-- Start + 1 because of [%s;] section
				RunTimeError('Malformed "try" section on [[%d]].', Start + 1);
			end;
		end;
		return PS - 1, (Symbol + 'Try(function()' + SpaceAfter + GrandStr:sub(Finish + 1, PS - 1) + 'end ');
		-- return str:gsub(pattern, 'Try(function() %1%2%3 end %4');
	end
);

HELLua.MacrosesMiscs = TList.New {
	Macro_Try;
};

HELLua.HMachine = TLua.Create();
local HMachine = HELLua.HMachine;

HMachine.Lexeme:Add(unpack( --HELLua.MacrosesPascal +
														HELLua.MacrosesCSyntax +
														HELLua.MacrosesCLogic +
														HELLua.MacrosesCMath +
														HELLua.MacrosesCTernary +
														HELLua.MacrosesEnhanced +
														HELLua.MacrosesMiscs +
														HELLua.MacrosesClass ));
-- HMachine.Lexeme:Add();
HMachine.Lexeme:AddScope( unpack( HELLua.ScopesList ) );

-- Miscs

function HELLua.Try(TryF, ExceptF, FinallyF) 
	local Handled = false;
	local Success = true;
	local Message = '';  
	xpcall(
	  	function() TryF(); end,
	  	function(Msg)
			Success = false;
			Message = Msg;
		  	if (type(ExceptF) == 'function') then
				ExceptF(Msg);
			  	Handled = true;
		  	end;
	  	end);
	if type(FinallyF) == 'function' then
	  	FinallyF();
	end;
	if not Success and not Handled then
	  	error(Message, 1);
	end;
end;

local GetEnvironment = getfenv;
local SetEnvironment = setfenv;
local SetMetaTable = setmetatable;
local GetMetaTable = getmetatable;

local CurrentStackLevel = 1;
local PreviousStackLevel = 2;

function HELLua.with(Object, doFunction)
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
		Object[index] = value;
	end;

	Environment.__ENV = OldEnv;
	Environment.self = Object;
	Environment.this = Object;

	Environment = SetMetaTable(Environment, EnvMeta);

	return SetEnvironment(doFunction, Environment)();
end;

function HELLua.strictly_with(Object, doFunction)
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

-- Returning

return HELLua;
