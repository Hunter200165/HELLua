 -- Classes library for HELLua (and Lua) Applications

--[[ Uses clause ]]  
    local bit = require 'bit'; 
    local HL = require 'HELLua_API'; 
    local HLC = require 'HELLua_ClassAPI';

local LocalEnv = {};
local Lib = {};

Import('.*', HLC.Class, LocalEnv);

with(LocalEnv, function() 
    with(Lib, function() 

        -- First, main and root class TObject. Please, inherit your classes from it.
        TObject = 
              CreateClass({ Public {  

                 }; Public {   Method { ["Name"] = "GetClassName" };
                 }; Public {   Method { ["Name"] = "GetParentName" };

                 }; Public {   Property { ["Name"] = "ClassName"  , ["Read"] = "GetClassName" };
                 }; Public {   Property { ["Name"] = "ClassParent"  , ["Read"] = "GetParentName" };

                 }; Public {   Method { ["Name"] = "InheritsFrom" };

                 }; Protected {   Field { ["Name"] = "FCreatedObject"  , ["Constant"] = true , ["Enclosed"] = true , ["Content"] = true };
                 }; Public {   Property { ["Name"] = "CreatedObject"  , ["Read"] = "FCreatedObject" , ["Enclosed"] = true };

                -- Constructors are public by default.
                 }; Public {   Method { ["Name"] = "Create", ["Constructor"] = true };
                 }; Public {   Method { ["Name"] = "New", ["Constructor"] = true };

             }; ["Implementation"] = function()

                -- FCreatedObject = false;

                function GetClassName()
                    return ____selfTopClass.__name;
                end;

                function GetParentName()
                    local Parent = ____selfTopClass.__parentPointer;
                    if (Parent ~= nil) then 
                        return Parent.__name;
                    else 
                        return nil;
                    end;
                end;

                function InheritsFrom(ClsName)
                    if (not (type(ClsName) == "string")) then return false; end;
                    local ClassInstance = ____selfTopClass;
                    while (ClassInstance ~= nil) do 
                        if ClassInstance.__name == ClsName then 
                            return true;
                        end;
                        ClassInstance = ClassInstance.__parentPointer;
                    end;
                    return false;
                end;

                -- Constructors!

                function Create()
                    -- It is empty.
                end;

                function New()
                    Create();
                end;

            end;}, "TObject");

        TException = 
                 CreateClass({ Public {  

                 }; Protected {   Method { ["Name"] = "Construct" };

                 }; Public {   Method { ["Name"] = "Create", ["Constructor"] = true };
                 }; Public {   Method { ["Name"] = "New", ["Constructor"] = true  , ["Linked"] = "Create" };

                 }; Protected {   Field { ["Name"] = "FMessage" };
                 }; Public {   Property { ["Name"] = "Message"  , ["Read"] = "FMessage" };

                 }; Public {   Method { ["Name"] = "Raise"  , ["Abstract"] = true , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "Throw"  , ["Abstract"] = true , ["Virtual"] = true };

             }; ["Implementation"] = function()

                FMessage = 'Unknown exception.';

                function Construct(str)
                    if ((type(str) == "string")) then 
                        FMessage = str;
                    end; 
                end;

                function Create(str)
                    Construct(str);
                end;

            end;}, "TException", "TObject");

        TFormattedException = 
                 CreateClass({ Public {  

                 }; Public {   Method { ["Name"] = "Create", ["Constructor"] = true };
                 }; Public {   Method { ["Name"] = "New", ["Constructor"] = true  , ["Linked"] = "Create" };

             }; ["Implementation"] = function()

                function Create(Msg, ...)
                    Construct(Msg:format(...));
                end;

            end;}, "TFormattedException", "TException");

        EFileException =      CreateClass({ Public {   }; }, "EFileException", "TFormattedException");

        _G.RawType = _G.type;
        _G.RawError = _G.error;
        _G.RawToString = _G.tostring;
        _G.RawToNumber = _G.tonumber;
        bit.RawBand = bit.band;
        bit.RawBor = bit.bor;
        bit.RawBxor = bit.bxor;
        bit.RawBnot = bit.bnot;
        bit.RawLShift = bit.lshift;
        bit.RawRShift = bit.rshift;

        _G.type = function(Var)
            local Typ = RawType(Var);
            if (Typ == 'table') and (Var.CreatedObject) then 
                return 'object';
            end; 
            return Typ;
        end;

        _G.tostring = function(Var)
            local Typ = type(Var);
            if (Typ == 'object') then 
                if ((RawType(Var.ToString) == "function")) then 
                    return Var.ToString();
                end;
                return 'object: ' + HL.ExtractTablePointer(RawToString(Var));
            end; 
            return RawToString(Var);
        end;

        local ExceptionMask = '[%s]: %s';

        _G.error = function(Msg, Level)
            if ((type(Msg) == "object") and (Msg.InheritsFrom("TException"))) then 
                _G.RawError(ExceptionMask:format(Msg.ClassName, Msg.Message), 1 + ((tonumber(Level)) and Level or 0));
            else 
                _G.RawError(Msg, (((tonumber(Level)) and Level or 0)) + 1);
            end;
        end;

        _G.tonumber = function(Var, Base)
            -- local Typ = typeof Var;
            if ((RawType(Var) == "table")) and ((RawType(Var.ToNumber) == "function")) then 
                return Var.ToNumber(Base);
            end;
            return RawToNumber(Var, Base);
        end;

        local CreateBitFunc = 
            function(Name, Func)
                return function(Op1, Op2, ...)
                    if ((RawType(Op1) == "table")) and ((RawType(Op1[Name]) == "function")) then return Op1[Name](Op1, Op2); end;
                    if ((RawType(Op2) == "table")) and ((RawType(Op2[Name]) == "function")) then return Op2[Name](Op1, Op2); end;
                    return Func(Op1, Op2, ...);
                end;
            end;

        bit.band = CreateBitFunc('OperatorBitwiseAnd', bit.RawBand);
        bit.bor = CreateBitFunc('OperatorBitwiseOr', bit.RawBor);
        bit.bxor = CreateBitFunc('OperatorBitwiseXor', bit.RawBxor);
        bit.lshift = CreateBitFunc('OperatorBitwiseShiftLeft', bit.RawLShift);
        bit.rshift = CreateBitFunc('OperatorBitwiseShiftRight', bit.RawRShift);
        bit.bnot = function(Op1) 
            if ((RawType(Op1) == "table")) and ((RawType(Op1.OperatorBitwiseNot) == "function")) then  
                return Op1.OperatorBitwiseNot(Op1);
            end;
            return bit.RawBnot(Op1);
        end;

        TString =
                 CreateClass({ Public {  

                 }; Public {   Method { ["Name"] = "ConcatenateStrings"  , ["Static"] = true };
                 }; Public {   Method { ["Name"] = "GetChar"  , ["Static"] = true };

                 }; Public {   Method { ["Name"] = "New"  , ["Static"] = true };

             }; ["Implementation"] = function()

                function ConcatenateStrings(str1, str2)
                    return str1 + str2;
                end;

                function GetChar(str, index)
                    return str:gsub(index, index);
                end;

                function New(Obj)
                    if ((type(Obj) == "nil")) then 
                        return '';
                    elseif ((type(Obj) == "table")) and ((type(Obj.ToString) == "function")) then 
                        return Obj.ToString();
                    else 
                        return tostring(Obj);
                    end;
                end;

            end;}, "TString", "TObject");

        TTable = 
                 CreateClass({ Public {  

                 }; Public {   Method { ["Name"] = "Join"  , ["Static"] = true };
                 }; Public {   Method { ["Name"] = "Insert"  , ["Static"] = true };
                 }; Public {   Method { ["Name"] = "Delete"  , ["Static"] = true };
                 }; Public {   Method { ["Name"] = "Length"  , ["Static"] = true };

                 }; Public {   Method { ["Name"] = "New"  , ["Static"] = true };

                 }; Public {   Method { ["Name"] = "Print"  , ["Static"] = true };

             }; ["Implementation"] = function()

                function New(...)
                    local Args = {...};
                    if #Args <= 0 then 
                        return {};
                    else 
                        return {unpack(Args)};
                    end;
                end;

                function Join(...)
                    local Args = {...};
                    local Result = {};
                    for i = 1, #Args do 
                        for k,v in pairs(Args[i]) do 
                            Result[k] = v;
                        end;
                    end;
                    return Result;
                end;

                function Length(Dict)
                    return #Dict;
                end;

                function Insert(Dict, Item, Position)
                    Position = (not (type(Position) == "number")) and (#Dict + 1) or Position;
                    local Len = #Dict;
                    if Position > Len then 
                        Dict[Position] = Item;
                        return Position;
                    else 
                        for i = #Dict, Position, -1 do 
                            Dict[i + 1] = Dict[i];
                        end; 
                        Dict[Position] = Item;
                        return Position;
                    end;
                end;

                function Delete(Dict, Position)
                    Position = (not (type(Position) == "number")) and (#Dict) or Position;
                    local Len = #Dict;
                    if Position >= Len then 
                        Dict[Position] = nil;
                    else
                        for i = Position, #Dict do 
                            Dict[i] = Dict[i + 1];
                        end; 
                    end;
                end;

                function Print(Op1, Name)
                    print (((((type(Name) == "string")) and Name or ('unnamed table'))) + ' = {');
                    for k,v in pairs(Op1) do 
                        print(('  %s = %s;'):format(tostring(k), tostring(v)));
                    end;
                    print '};';
                end;

            end;}, "TTable", "TObject");

        TNumber = 
                 CreateClass({ Public {  

                 }; Protected {   Field { ["Name"] = "FContent"  , ["Content"] = 0 };
                 }; Protected {   Method { ["Name"] = "SetContent" };

                 }; Public {   Property { ["Name"] = "Content"  , ["Read"] = "FContent" , ["Write"] = "SetContent" };

                 }; Public {   Method { ["Name"] = "Create", ["Constructor"] = true };
                 }; Public {   Method { ["Name"] = "New", ["Constructor"] = true  , ["Linked"] = "Create" };

                 }; Public {   Method { ["Name"] = "ToString" };
                 }; Public {   Method { ["Name"] = "ToNumber" };
                 }; Public {   Method { ["Name"] = "Print" };

                 }; Public {   Method { ["Name"] = "OperatorAdd"  , ["Static"] = true , ["Operator"] = true };
                 }; Public {   Method { ["Name"] = "OperatorMultiply"  , ["Static"] = true , ["Operator"] = true };

                 }; Public {   Method { ["Name"] = "OperatorBitwiseAnd"  , ["Static"] = true , ["Operator"] = true };
                 }; Public {   Method { ["Name"] = "OperatorBitwiseOr"  , ["Static"] = true , ["Operator"] = true };
                 }; Public {   Method { ["Name"] = "OperatorBitwiseNot"  , ["Static"] = true , ["Operator"] = true };

             }; ["Implementation"] = function()

                -- print(TObject);

                function ToString()
                    return tostring(self.FContent);
                end;

                function ToNumber()
                    return FContent;
                end;

                function Print()
                    -- print('Self: ' + self); 
                    print(FContent);
                end;

                function SetContent(Value)
                    local A = tonumber(Value);
                    if A == nil then 
                    else
                        self.FContent = A; 
                    end;
                end;

                function Create(Value)
                    if Value ~= nil then 
                        self.SetContent(Value);
                    end;
                end;

                function OperatorAdd(Op1, Op2)
                    if not ((type(Op1) == "object") and (Op1.InheritsFrom("TNumber"))) then Op1 = TNumber.Create(Op1); end;
                    if not ((type(Op2) == "object") and (Op2.InheritsFrom("TNumber"))) then Op2 = TNumber.Create(Op2); end;
                    return TNumber.Create(Op1.Content + Op2.Content);
                end;

                function OperatorMultiply(Op1, Op2)
                    if not ((type(Op1) == "object") and (Op1.InheritsFrom("TNumber"))) then Op1 = TNumber.Create(Op1); end;
                    if not ((type(Op2) == "object") and (Op2.InheritsFrom("TNumber"))) then Op2 = TNumber.Create(Op2); end;
                    return TNumber.Create(Op1.Content * Op2.Content);
                end;

                function OperatorBitwiseAnd(Op1, Op2)
                    if ((type(Op1) == "object") and (Op1.InheritsFrom("TNumber"))) then 
                        return (bit.band(Op1.Content, Op2));
                    else 
                        return (bit.band(Op2.Content, Op1));
                    end;
                end;

                function OperatorBitwiseOr(Op1, Op2)
                    if ((type(Op1) == "object") and (Op1.InheritsFrom("TNumber"))) then 
                        return (bit.bor(Op1.Content, Op2));
                    else 
                        return (bit.bor(Op2.Content, Op1));
                    end;
                end;

                function OperatorBitwiseNot(Op1)
                    return (bit.bnot(Op1.Content));
                end;

            end;}, "TNumber", "TObject");

        TStopWatch = 
                 CreateClass({ Public {  

                 }; Protected {   Field { ["Name"] = "FStartTime"  , ["Content"] = 0 };
                 }; Protected {   Field { ["Name"] = "FStopTime"  , ["Content"] = 0 };

                 }; Protected {   Field { ["Name"] = "FStarted"  , ["Content"] = false };
                 }; Public {   Property { ["Name"] = "Started"  , ["Read"] = "FStarted" };
                 }; Public {   Property { ["Name"] = "Stopped"  , ["Read"] = "FStopTime" };

                 }; Public {   Method { ["Name"] = "GetCurrentInterval" };
                 }; Protected {   Method { ["Name"] = "GetTime" };
                 }; Public {   Property { ["Name"] = "Time"  , ["Read"] = "GetTime" };

                 }; Public {   Method { ["Name"] = "Start" };
                 }; Public {   Method { ["Name"] = "Stop" };

                 }; Public {   Method { ["Name"] = "Create", ["Constructor"] = true };
                 }; Public {   Method { ["Name"] = "New", ["Constructor"] = true  , ["Linked"] = "Create" };

             }; ["Implementation"] = function()

                function Start()
                    if FStarted then error('Already started!'); end;
                    FStartTime = os.clock() * 1000;
                    FStarted = true;
                end;

                function Stop()
                    if not FStarted then error('Not started!'); end;
                    FStarted = false;
                    FStopTime = os.clock() * 1000;
                    return GetTime();
                end;

                function GetTime()
                    return FStopTime - FStartTime;
                end;

                function GetCurrentInterval()
                    return os.clock() * 1000 - FStartTime;
                end;

                function Create(ShouldStart)
                    if ((type(ShouldStart) == "boolean")) and ShouldStart then 
                        Start();
                    end;
                end;

            end;}, "TStopWatch", "TObject");

        TCallback = 
                 CreateClass({ Public {  

                 }; Protected {   Field { ["Name"] = "FCallbacks" };
                 }; Public {   Property { ["Name"] = "Callbacks"  , ["Read"] = "FCallbacks" };

                 }; Public {   Method { ["Name"] = "RaiseEvents" };
                 }; Public {   Method { ["Name"] = "AddEvent" };
                 }; Public {   Method { ["Name"] = "OperatorAdd"  , ["Static"] = true , ["Operator"] = true };

             }; ["Implementation"] = function()

                FCallbacks = {};

                function RaiseEvents(...)
                    for i = 1, #FCallbacks do 
                        FCallbacks[i](...);
                    end;
                end;

                function AddEvent(Event)
                    return TTable.Insert(FCallbacks, Event);
                end;

                function OperatorAdd(Op1, Op2)
                    if ((RawType(Op1) == "function")) then 
                        Op2.AddEvent(Op1);
                        return Op2; 
                    end;
                    if ((RawType(Op2) == "function")) then 
                        Op1.AddEvent(Op2);
                        return Op1; 
                    end;
                    error('Only function type can be added to TCallback class.');
                end;

            end;}, "TCallback", "TObject");

        TInputOutput = 
                 CreateClass({ Public {  

                 }; Public {   Method { ["Name"] = "WriteLn"  , ["Abstract"] = true , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "Write"  , ["Abstract"] = true , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "ReadLn"  , ["Abstract"] = true , ["Virtual"] = true };

				 }; Protected {   Field { ["Name"] = "ClassesLib"  , ["Content"] = Lib };
				
                 }; Protected {   Field { ["Name"] = "FOnWrite" };
                 }; Protected {   Field { ["Name"] = "FOnWriteLn" };
                 }; Protected {   Field { ["Name"] = "FOnReadLn" };

                 }; Public {   Property { ["Name"] = "OnWrite"  , ["Read"] = "FOnWrite" , ["Write"] = "FOnWrite" };
                 }; Public {   Property { ["Name"] = "OnWriteLn"  , ["Read"] = "FOnWriteLn" , ["Write"] = "FOnWriteLn" };
                 }; Public {   Property { ["Name"] = "OnReadLn"  , ["Read"] = "FOnReadLn" , ["Write"] = "FOnReadLn" };

                 }; Public {   Method { ["Name"] = "OperatorBitwiseShiftLeft" };
                 }; Public {   Method { ["Name"] = "OperatorBitwiseShiftRight" }; 

                 }; Protected {   Field { ["Name"] = "PrintF" };
                 }; Protected {   Field { ["Name"] = "WriteF" };
                 }; Protected {   Field { ["Name"] = "ReadF" };
                 }; Protected {   Field { ["Name"] = "CloseF" };
                 }; Protected {   Field { ["Name"] = "FlushF" };
                 }; Protected {   Field { ["Name"] = "OpenF" };

                 }; Public {   Method { ["Name"] = "Create", ["Constructor"] = true };
                 }; Public {   Method { ["Name"] = "New", ["Constructor"] = true  , ["Linked"] = "Create" };

             }; ["Implementation"] = function()

                PrintF = print;
                WriteF = io.write;
                ReadF = io.read;
                CloseF = io.close;
                FlushF = io.flush;
                OpenF = io.open;

                function OperatorBitwiseShiftLeft(OprndF, OprndS)
                    if (type(OprndF) == "object") and (OprndF.InheritsFrom("TInputOutput")) then OprndF.Write(OprndS); end;
                    if (type(OprndS) == "object") and (OprndS.InheritsFrom("TInputOutput")) then return OprndS.ReadLn(OprndF); end;
                    return OprndF;
                end;

                function OperatorBitwiseShiftRight(OpF, OpS)
                    if (type(OpF) == "object") and (OpF.InheritsFrom("TInputOutput")) then return OpF.ReadLn(OpS); end;
                    if (type(OpS) == "object") and (OpS.InheritsFrom("TInputOutput")) then OpS.Write(OpF); end;
                    return OpS;
                end;

                function Create()
                    FOnWrite = ClassesLib.TCallback.New();
                    FOnWriteLn = ClassesLib.TCallback.New();
                    FOnReadLn = ClassesLib.TCallback.New();
                end;

            end;}, "TInputOutput", "TObject");

        TConsole =
                 CreateClass({ Public {  

                 }; Public {   Method { ["Name"] = "WriteLn"  , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "Write"  , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "ReadLn"  , ["Virtual"] = true };

             }; ["Implementation"] = function()

                function WriteLn(...)
                    FOnWriteLn.RaiseEvents(...);
                    PrintF(...);
                end;

                function Write(...)
                    FOnWrite.RaiseEvents(...);
                    WriteF(...);
                end;

                function ReadLn(...)
                    local Result = {io.read(...)};
                    FOnReadLn.RaiseEvents(unpack(Result));
                    return unpack(Result);
                end;

            end;}, "TConsole", "TInputOutput");

        fmOpenWrite = 1;
        fmOpenRead  = 2;
        fmOpenAppend = 4;
        fmOpenReadWrite = (bit.bor(fmOpenWrite, fmOpenRead));
        fmOpenWriteAdvanced = (bit.bor(fmOpenWrite, 128));
        fmOpenReadAdvanced  = (bit.bor(fmOpenRead, 128));
        fmOpenAppendAdvanced = (bit.bor(fmOpenAppend, 128));

        TFile = 
                 CreateClass({ Public {  

                 }; Public {   Field { ["Name"] = "FFile" }; --: userdata
                 }; Protected {   Field { ["Name"] = "FMode" }; --: String
                 }; Public {   Property { ["Name"] = "FileOpened"  , ["Read"] = "FFile" , ["Write"] = "FFile" };
                 }; Public {   Property { ["Name"] = "Mode"  , ["Read"] = "FMode" };

                 }; Public {   Method { ["Name"] = "WriteLn"  , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "Write"  , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "ReadLn"  , ["Virtual"] = true };

                 }; Public {   Method { ["Name"] = "Flush"  , ["Virtual"] = true };

                 }; Protected {   Method { ["Name"] = "CheckOpened"  , ["Virtual"] = true };

                 }; Protected {   Field { ["Name"] = "FClosed"  , ["Content"] = true };
                 }; Public {   Property { ["Name"] = "Closed"  , ["Read"] = "FClosed" };

                 }; Public {   Method { ["Name"] = "Close"  , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "Seek"  , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "SetPosition"  , ["Virtual"] = true };
                 }; Public {   Method { ["Name"] = "SetPositionFromStart"  , ["Virtual"] = true };

                 }; Public {   Property { ["Name"] = "Position"  , ["Read"] = "Seek" , ["Write"] = "SetPositionFromStart" };

                 }; Public {   Method { ["Name"] = "Create", ["Constructor"] = true };
                 }; Public {   Method { ["Name"] = "New", ["Constructor"] = true  , ["Linked"] = "Create" };

             }; ["Implementation"] = function()

                FMode = '';

                function Create(FileName, FileMode)
                    if (not (RawType(FileMode) == "number")) then error(EFileException.Create('File mode type is not valid. Expected Number.')); end;
                    if (not (RawType(FileName) == "string")) then error(EFileException.Create('File name type is not valid. Expected String.')); end;
                    if FileMode == fmOpenWrite then FMode = 'w';
                    elseif FileMode == fmOpenAppend then FMode = 'a';
                    elseif FileMode == fmOpenRead then FMode = 'r';
                    elseif FileMode == fmOpenWriteAdvanced then FMode = 'w+';
                    elseif FileMode == fmOpenAppendAdvanced then FMode = 'a+';
                    elseif FileMode == fmOpenReadAdvanced then FMode = 'r+';
                    elseif FileMode == fmOpenReadWrite then FMode = 'wr';
                    else
                        error(EFileException.Create('File mode is not valid: %s', tostring(FileMode))); 
                    end;
                    local File, Msg = OpenF(FileName, FMode);
                    if not (type(File) == "userdata") then 
                        error(EFileException.Create('Cannot open file "%s": %s', tostring(FileName), tostring(Msg)));
                    end;
                    FFile = File;
                    FClosed = false;
                    --[[ Do inherited ]]
                    FOnWrite = TCallback.New();
                    FOnWriteLn = TCallback.New();
                    FOnReadLn = TCallback.New();
                end;

                function CheckOpened()
                    if Closed then 
                        error(EFileException.Create('Attempt to work with closed file.'));
                    end;
                end;

                function WriteLn(...)
                    CheckOpened();
                    FOnWriteLn.RaiseEvents(...); 
                    local Arr = {...};
                    for i = 1, #Arr do Arr[i] = tostring(Arr[i]); end;
                    return FFile:write(table.concat(Arr), '\n');
                end;

                function Write(...)
                    CheckOpened();
                    FOnWrite.RaiseEvents(...); 
                    local Arr = {...};
                    for i = 1, #Arr do Arr[i] = tostring(Arr[i]); end;
                    return FFile:write(table.concat(Arr));
                end;

                function ReadLn(...)
                    CheckOpened();
                    local Result = {FFile:read(...)};
                    FOnReadLn.RaiseEvents(unpack(Result));
                    return unpack(Result);
                end;

                function Close()
                    CheckOpened();
                    FClosed = true;
                    FFile:close();
                    FFile = nil;
                end;

                function Flush()
                    CheckOpened();
                    FFile:flush();
                end;

                function Seek()
                    CheckOpened();
                    return FFile:seek();
                end;

                function SetPosition(...)
                    CheckOpened();
                    return FFile:seek(...);
                end;

                function SetPositionFromStart(pos)
                    return SetPosition('set', pos);
                end;

            end;}, "TFile", "TInputOutput");

        TGarbageCollector = 
                 CreateClass({ Public {  

                 }; Public {   Method { ["Name"] = "Collect" };
                 }; Public {   Method { ["Name"] = "GetRAMUsage" };

                 }; Protected {   Field { ["Name"] = "CollectGarbageF" };

             }; ["Implementation"] = function()

                CollectGarbageF = collectgarbage;

                function GetRAMUsage()
                    -- Returns in Bytes
                    return CollectGarbageF('count') * 1024;
                end;

                function Collect()
                    local Old = GetRAMUsage();
                    CollectGarbageF 'collect';
                    local New = GetRAMUsage();
                    return Old - New;
                end;

            end;}, "TGarbageCollector", "TObject");

    end);
end);

return Lib;
