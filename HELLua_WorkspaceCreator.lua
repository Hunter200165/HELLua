 --[[
    Creator of workspaces.
]]

local CommandCreateDir = 'mkdir %s';
local SuccessCode = 0;

local Write = io.write;

function CreateFile(FileName, Content)
    Write ((' - Trying to create file "%s"... '):format(FileName));
    local Fil = io.open(FileName, 'w');
    if (type(Fil) == "nil") then 
        error(('Cannot create file "%s"'):format(FileName), -1);
    end;
    print 'Success.';
    if (not (type(Content) == "string")) then 
        Fil:close();
        return nil; 
    end;
    Fil:write(Content);
    Fil:close();
end;

function Main(WName, WFilename, WBuilder)
    local NAuto = not (type(WName) == "string");
    Write 'Name of folder: ';
    local Name = nil;
    if NAuto then 
        Name = io.read();
    else 
        Name = WName;
        print(Name);
    end;
    local Cmd = CommandCreateDir:format(Name);
    Write((' - Creating folder (Executing "%s")... '):format(Cmd));
    local Success = os.execute(Cmd);
    if Success ~= SuccessCode then 
        -- error(('Cannot create folder "%s"'):format(), -1);
				io.write('Maybe it was created?');
    end;
    print ' - Created folder.';
    Write (('Name of main file without extension (Leave blank for: "%s"):\n> '):format(Name .. '.hlua'));
    local FileName = nil;
    if NAuto then 
        FileName = io.read();
    else 
        FileName = ((type(WFilename) == "string") and (WFilename) or (('')));
        print(FileName);
    end;
    if FileName == '' then FileName = Name; end;
    local FileNameF = Name .. '\\' .. FileName .. '.hlua'; 
    CreateFile(FileNameF);
    Write (('Name of builder file without extension (Leave blank for: "%s"):\n> '):format('Build_' .. FileName .. '.cmd'));
    local BuildFileName = nil;
    if NAuto then 
        BuildFileName = io.read();
    else
        BuildFileName = ((type(WBuilder) == "string") and (WBuilder) or (('')));
        print(BuildFileName);
    end;
    if BuildFileName == '' then BuildFileName = 'Build_' .. FileName; end;
    local BuildFileNameF = Name .. '\\' .. BuildFileName .. '.cmd';
    CreateFile(BuildFileNameF, ('cd ..\nHELLua_Builder.lua %s\\%s\npause'):format(Name, FileName .. '.hlua'));
    print 'Done.';
    io.read();
end;

local Args = {...}

Main(unpack(Args));
