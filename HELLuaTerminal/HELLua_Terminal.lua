--[[
	HELLua Terminal written by Hunter200165;
	Based on HELLua - HEnhanced Lexeme Lua compiler
]]

local HELLua = require 'HELLua';

local HEnv = {Try = HELLua.Try, with = HELLua.with, strictly_with = HELLua.strictly_with};

local DebugTextOut = false;

local Engine = HELLua.DeepCopy(HELLua.HMachine);
local WString = Engine:ProcessText(
	[[
		-- HELLua script for internal management

		local Terminal = {};

		with Terminal do

			function Execute(Line)
				try
					local Suc, Msg = loadstring(Line, 'HELLua-Shell');
					if not Suc then
						error(Msg);
					end;
					HELLua.with(HEnv, Suc);
					-- Suc();
				except on E do
					print(E);
					print(debug.traceback('', 2));
				end;
			end;

			function Main()
				print('HELLua Command Line Interpreter. Copyright (c) Hunter200165, 2018');
				while (true) do
					io.write('> ');
					local Msg = io.read();
					if Msg == 'exit!' then break; end;
					if Msg == 'multln!' then 
						Msg = '';
						repeat
							io.write '#> ';
							local Read = io.read();
							if Read == 'end!' then break; end;
							Msg += (Read + '\n');
						until false;
					end;
					try
						local PrintMsg = false;
						if Msg[1] == '=' then
							Msg = Msg:gsub('[%s;]*$', '');
							Msg = ('print(%s)'):format(Msg:sub(2));
						elseif Msg[1] == '$' then
							Msg = ('print(type(%s))'):format(Msg:sub(2));
						elseif Msg:sub(1, 3) == 'out' then
							PrintMsg = true;
							Msg = Msg:sub(4);
						end;
						Msg = Engine:ProcessText(Msg);
						if not PrintMsg then 
							Execute(Msg);
						else
							print(Msg);
						end;
					except on e do
						print(e);
					end;
				end;
			end;

			Main();

		end;

	]]
);

local Succ, Msg = loadstring(WString, 'HELLua_Shell');

if not Succ then
	print('[Error]: ' + Msg);
	if DebugTextOut then
		print('Text: \n', WString);
	end;
	return nil;
end;

local SSC, SMsg = pcall(function()
	HELLua.with(HEnv, Succ);
end);

if not SSC then
	print('[Error]: ' + SMsg);
	if DebugTextOut then
		print('Text: \n', WString);
	end;
	return nil;
end;
