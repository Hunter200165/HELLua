 -- HELLua class API

--${CHECKS -}

--[[ Uses clause ]]  
	local HL = require 'HELLua_API';
	
local HELLua_Class = {};

ErrorMask = '[HELLua_ClassAPI][Error]: %s';
	
Errors = {
	EUnexpectedType = ErrorMask:format 'Unexpected type %s. Expected: %s';
	EPropertyIsUnreachable = ErrorMask:format 'Property needs to have either "Read" or "Write" field.';
	ENameIsInvalid = ErrorMask:format 'Provided name is invalid or has invalid type.';
	ENameDuplication = ErrorMask:format 'Name "%s" is duplicating in class definition.';
	EUnknownType = ErrorMask:format 'Unknown type is provided: %s';
	EPropertySetterIsInvalid = ErrorMask:format 'Property setter is invalid: %s';
	EPropertyGetterIsInvalid = ErrorMask:format 'Property getter is invalid: %s';
	EImplementedMethodIsNotDefined = ErrorMask:format 'Method "%s" is implemented, but has no definition.';
	EMethodDefinedIsNotImplemented = ErrorMask:format 'Method "%s" is defined, but has no implementation.';
	EUnknownVisibility = ErrorMask:format 'Unknown visibility value is provided: %s';
	EUndefinedFieldAccess = ErrorMask:format 'Trying to access undefined field: %s';
	ECannotReadWriteOnlyProperty = ErrorMask:format 'Cannot read write-only property: %s';
	ECannotWriteReadOnlyProperty = ErrorMask:format 'Cannot write read-only property: %s';
	EPropertyHasInvalidGetter = ErrorMask:format 'Property "%s" has invalid or corrupted getter "%s"';
	EPropertyHasInvalidSetter = ErrorMask:format 'Property "%s" has invalid or corrupted setter "%s"';
	ECannotAssignValueToMethod = ErrorMask:format 'Cannot assing value to method field "%s"';
	EConstructorCannotBeStatic = ErrorMask:format 'Constructor method "%s" cannot be static.';
	EConstructorCannotBePrivate = ErrorMask:format 'Constructor method "%s" cannot be private.';
	EParentClassIsNotDefined = ErrorMask:format 'Parent class is not defined: %s';
	EClassNameDuplication = ErrorMask:format 'Class name is duplicating: %s';
	EMethodIsNotInherited = ErrorMask:format 'Method "%s" is not inherited.';
	EMethodTypeIsViolated = ErrorMask:format 'Method type of "%s" is violated. Expected function, but got: %s';
	EPropertyImplementationIsNotAllowed = ErrorMask:format 'Property assignation is not allowed under implementation field.';
	EImplementedFieldHasNoDefinition = ErrorMask:format 'Field "%s" is assigned to value, but has no explicit implementation.';
	ENoConstructorsAreDefined = ErrorMask:format 'Class has no available constructors.';
	ELinkedMethodHasNoEndLink = ErrorMask:format 'Method "%s" has a link, but redirection to "%s" was failed.';
	ELinkedMethodCannotHaveImplementation = ErrorMask:format 'Linked method "%s" cannot be implemented.';
	ELinkedMethodHasInvalidDefinition = ErrorMask:format 'Linked method "%s" differs from initial method "%s".';
	ELinkedMethodLinkingPointer = ErrorMask:format 'Linked method "%s" is pointing to linked method "%s".';
	EStaticMethodCannotBeVirtual = ErrorMask:format 'Static method "%s" cannot be virtual.';
	EConstantFieldMustHaveValue = ErrorMask:format 'Constant field "%s" must have a value.';
	EVirtualMethodCannotBeEnclosed = ErrorMask:format 'Virtual method "%s" cannot be enclosed.';
	EAbstractMethodMustBeVirtual = ErrorMask:format 'Abstract method "%s" must be virtual.';
	EConstructorCannotBeAbstract = ErrorMask:format 'Constructor "%s" cannot be abstract.';
	EConstructorCannotBeEnclosed = ErrorMask:format 'Constructor "%s" cannot be enclosed.';
	EPartTypeCannotBeOverriden = ErrorMask:format 'Part "%s" cannot change type from "%s" to "%s".';
	EEnclosedPartCannotBeOverriden = ErrorMask:format 'Enclosed part "%s" cannot be overriden.';
	EPropertyCannotWriteToConstantField = ErrorMask:format 'Property "%s" cannot write to constant field "%s".';
	ECannotChangeConstantField = ErrorMask:format 'Cannot change constant field "%s".';
	EAbstractError = ErrorMask:format 'Trying to call abstract method "%s" which has no implementation.';
	ENativePropertyMustBeIndexed = ErrorMask:format 'Native property "%s" must be indexed.';
	ENativePropertyCanBeOnlyOne = ErrorMask:format 'Native property "%s" cannot change primary native property "%s".';
	EIndexedPropertyCannotBeAssigned = ErrorMask:format 'Indexed property "%s" cannot be assigned.';
	EOperatorCannotBeModified = ErrorMask:format 'Operator "%s" cannot have other modifiers.';
	EOperatorMustBeStatic = ErrorMask:format 'Operator "%s" must be static.';
	EOperatorMustBePublic = ErrorMask:format 'Operator "%s" must be public.';
};

Visibility_Public  = 0;
Visibility_Private = 1;

Type_Field = 0;
Type_Property = 1;
Type_Method = 2;

ClassesTree = {};

local type = type;
local tostring = tostring;
local tonumber = tonumber;

local function ConstructClass(Fields, Properties, Methods, Envir, RawClass)
 
	Fields = HL.DeepCopy(Fields);
	Properties = HL.DeepCopy(Properties);
	Methods = HL.DeepCopy(Methods);

	Fields = HELLua_Class.Class.ArrayToTable(Fields, 'Name');
	Properties = HELLua_Class.Class.ArrayToTable(Properties, 'Name');
	Methods = HELLua_Class.Class.ArrayToTable(Methods, 'Name');

	if not Inert then for k,v in pairs(Methods) do if (not (v.Abstract or v.Static)) and ((type(v.Function) == "function")) then 
		v.Function = HL.CopyFunction(v.Function);
	end; end; end;
	
	local PublicScope = {};
	local PrivateScope = {};
	local Operators = {};

	local TpmList = {Fields, Properties, Methods};
	for _, TableP in pairs(TpmList) do 
		for k,v in pairs(TableP) do 
			PrivateScope[k] = v;
			if v.Visibility ~= Visibility_Private then PublicScope[k] = v; end;
			if (v.Type == Type_Method) and (v.Operator) then Operators[k] = v; end;
		end;
	end;

	local function PropertyGetter(PropObj)
		local Read = PropObj.Read;
		local Objecto = PrivateScope[Read];
		if (Objecto == nil) or (Objecto.Type == Type_Property) then error(Errors.EPropertyGetterIsInvalid:format(tostring(Read))); end;
		if Objecto.Type == Type_Field then 
			return Objecto.Content;
		elseif Objecto.Type == Type_Method then 
			if Objecto.Abstract then error(Errors.EAbstractError:format(tostring(Read))); end;
			return Objecto.Function();
		else 
			error(Errors.EUnknownType:format(tostring(Objecto.Type)));
		end;
	end;

	local function PropertySetter(PropObj, Value)
		local Write = PropObj.Write;
		local Objecto = PrivateScope[Write];
		if (Objecto == nil) or (Objecto.Type == Type_Property) then error(Errors.EPropertySetterIsInvalid:format(tostring(Write))); end;
		if Objecto.Type == Type_Field then 
			if Objecto.Constant then error(Errors.ECannotChangeConstantField:format(tostring(Write))); end;
			Objecto.Content = Value;
		elseif Objecto.Type == Type_Method then 
			if Objecto.Abstract then error(Errors.EAbstractError:format(tostring(Write))); end;
			Objecto.Function(Value);
		else 
			error(Errors.EUnknownType:format(tostring(Objecto.Type)));
		end;
	end;

	local PrevObject = { 
		Fields = Fields; 
		Properties = Properties; 
		Methods = Methods; 
		PublicScope = PublicScope; 
		PrivateScope = PrivateScope;
	};

	local InnerEnvMeta = {};
	local InnerEnvMetas, OldInnerEnv = Envir.InnerEnvMeta, Envir.OldInnerEnv;

	local InIndexFunction = InnerEnvMetas.__index;
	local InNewIndexFunction = InnerEnvMetas.__newindex;

	InnerEnvMeta.__index = function(self, index)
		local Objecto = PrivateScope[index];
		if Objecto == nil then return InIndexFunction(self, index); end;
		if Objecto.Type == Type_Field then 
			return Objecto.Content;
		elseif Objecto.Type == Type_Method then 
			if Objecto.Abstract then error(Errors.EAbstractError:format(tostring(index))); end;
			return Objecto.Function;
		elseif Objecto.Type == Type_Property then 
			return PropertyGetter(Objecto);
		else 
			error(Errors.EUnknownType:format(tostring(Objecto.Type)));
		end;
	end;

	InnerEnvMeta.__newindex = function(self, index, Value)
		local Objecto = PrivateScope[index];
		if Objecto == nil then return InNewIndexFunction(self, index, Value); end;
		if Objecto.Type == Type_Field then 
			if Objecto.Constant then error(Errors.ECannotChangeConstantField:format(tostring(index))); end;
			Objecto.Content = Value;
			return true;
		elseif Objecto.Type == Type_Property then 
			PropertySetter(Objecto, Value);
			return true;
		elseif Objecto.Type == Type_Method then 
			error(Errors.ECannotAssignValueToMethod:format(tostring(index)));
		else
			error(Errors.EUnknownType:format(tostring(Objecto.Type))); 
		end;
	end;

	local PublicClassPattern = {};
	local PrivateClassPattern = {};

	local PublicEnv = {};
	local PrivateEnv = {};

	PublicEnv.__index = function(self, index) 
		local Objecto = PublicScope[index];
		if Objecto == nil then return nil; end;
		if Objecto.Type == Type_Field then 
			return Objecto.Content;
		elseif Objecto.Type == Type_Method then 
			if Objecto.Abstract then error(Errors.EAbstractError:format(tostring(Name))); end;
			return Objecto.Function;
		elseif Objecto.Type == Type_Property then 
			return PropertyGetter(Objecto);
		else 
			error(Errors.EUnknownType:format(tostring(Objecto.Type)));
		end;
	end;
	PublicEnv.__newindex = function(self, index, value) 
		local Objecto = PublicScope[index];
		if Objecto == nil then return nil; end;
		if Objecto.Type == Type_Field then 
			if Objecto.Constant then error(Errors.ECannotChangeConstantField:format(tostring(Name))); end;
			Objecto.Content = value;
			return true;
		elseif Objecto.Type == Type_Property then 
			PropertySetter(Objecto, value);
			return true;
		elseif Objecto.Type == Type_Method then 
			error(Errors.ECannotAssignValueToMethod:format(tostring(Name)));
		else
			error(Errors.EUnknownType:format(tostring(Objecto.Type))); 
		end;
	end;

	if ((type(Operators.OperatorAdd) == "table")) then PublicEnv.__add = Operators.OperatorAdd.Function; end;
	if ((type(Operators.OperatorSubtract) == "table")) then PublicEnv.__sub = Operators.OperatorSubtract.Function; end;
	if ((type(Operators.OperatorMultiply) == "table")) then PublicEnv.__mul = Operators.OperatorMultiply.Function; end;
	if ((type(Operators.OperatorDivide) == "table")) then PublicEnv.__div = Operators.OperatorDivide.Function; end;
	if ((type(Operators.OperatorModulo) == "table")) then PublicEnv.__mod = Operators.OperatorModulo.Function; end;
	if ((type(Operators.OperatorPower) == "table")) then PublicEnv.__pow = Operators.OperatorPower.Function; end;
	if ((type(Operators.OperatorNegate) == "table")) then PublicEnv.__unm = Operators.OperatorNegate.Function; end;
	if ((type(Operators.OperatorConcatenate) == "table")) then PublicEnv.__concat = Operators.OperatorConcatenate.Function; end;
	if ((type(Operators.OperatorLength) == "table")) then PublicEnv.__len = Operators.OperatorLength.Function; end;
	if ((type(Operators.OperatorEquals) == "table")) then PublicEnv.__eq = Operators.OperatorEquals.Function; end;
	if ((type(Operators.OperatorLessThan) == "table")) then PublicEnv.__lt = Operators.OperatorLessThan.Function; end;
	if ((type(Operators.OperatorLessOrEqualThan) == "table")) then PublicEnv.__le = Operators.OperatorLessOrEqualThan; end;

	PrivateEnv.__index = function(self, index) 
		local Objecto = PrivateScope[index];
		if Objecto == nil then return nil; end;
		if Objecto.Type == Type_Field then 
			return Objecto.Content;
		elseif Objecto.Type == Type_Method then 
			if Objecto.Abstract then error(Errors.EAbstractError:format(tostring(index))); end;
			return Objecto.Function;
		elseif Objecto.Type == Type_Property then 
			return PropertyGetter(Objecto);
		else 
			error(Errors.EUnknownType:format(tostring(Objecto.Type)));
		end;
	end;
	PrivateEnv.__newindex = function(self, index, Value) 
		local Objecto = PrivateScope[index];
		if Objecto == nil then return nil; end;
		if Objecto.Type == Type_Field then 
			if Objecto.Constant then error(Errors.ECannotChangeConstantField:format(tostring(index))); end;
			Objecto.Content = Value;
			return true;
		elseif Objecto.Type == Type_Property then 
			PropertySetter(Objecto, Value);
			return true;
		elseif Objecto.Type == Type_Method then 
			error(Errors.ECannotAssignValueToMethod:format(tostring(index)));
		else
			error(Errors.EUnknownType:format(tostring(Objecto.Type))); 
		end;
	end;

	if ((type(Operators.OperatorAdd) == "table")) then PrivateEnv.__add = Operators.OperatorAdd.Function; end;
	if ((type(Operators.OperatorSubtract) == "table")) then PrivateEnv.__sub = Operators.OperatorSubtract.Function; end;
	if ((type(Operators.OperatorMultiply) == "table")) then PrivateEnv.__mul = Operators.OperatorMultiply.Function; end;
	if ((type(Operators.OperatorDivide) == "table")) then PrivateEnv.__div = Operators.OperatorDivide.Function; end;
	if ((type(Operators.OperatorModulo) == "table")) then PrivateEnv.__mod = Operators.OperatorModulo.Function; end;
	if ((type(Operators.OperatorPower) == "table")) then PrivateEnv.__pow = Operators.OperatorPower.Function; end;
	if ((type(Operators.OperatorNegate) == "table")) then PrivateEnv.__unm = Operators.OperatorNegate.Function; end;
	if ((type(Operators.OperatorConcatenate) == "table")) then PrivateEnv.__concat = Operators.OperatorConcatenate.Function; end;
	if ((type(Operators.OperatorLength) == "table")) then PrivateEnv.__len = Operators.OperatorLength.Function; end;
	if ((type(Operators.OperatorEquals) == "table")) then PrivateEnv.__eq = Operators.OperatorEquals.Function; end;
	if ((type(Operators.OperatorLessThan) == "table")) then PrivateEnv.__lt = Operators.OperatorLessThan.Function; end;
	if ((type(Operators.OperatorLessOrEqualThan) == "table")) then PrivateEnv.__le = Operators.OperatorLessOrEqualThan; end;

	PublicClassPattern = setmetatable(PublicClassPattern, PublicEnv);
	PrivateClassPattern = setmetatable(PrivateClassPattern, PrivateEnv);

	for k,v in pairs(Methods) do 
		if not (v.Abstract or v.Static) then 
			local Env = {};
			Env.self = PrivateClassPattern;
			Env.this = PrivateClassPattern;
			Env.LinkedInstance = PublicClassPattern;
			Env.__OldENV = OldInnerEnv;
			Env.____selfPointerToMethod = v;
			Env.____selfOwnerBaseClass = v.Owner;
			Env.____selfTopClass = RawClass;
			Env.Inherited = Inherited;
			v.Function = setfenv(v.Function, setmetatable(Env, InnerEnvMeta));
		end;
	end;

	return 
		PublicClassPattern, 
		PrivateClassPattern,
		InnerEnvMeta, 
		OldInnerEnv;

end;

local function ClassConstructionMethod(__fields, __properties, __methods, Envir, RawClass)

	local __fields = HL.DeepCopy(__fields);
	local __properties = HL.DeepCopy(__properties);
	local __methods = HL.DeepCopy(__methods);

	for i = 1, #__fields do 
		__fields[__fields[i].Name] = __fields[i];
		__fields[i] = nil;
	end;

	for i = 1, #__properties do 
		__properties[__properties[i].Name] = __properties[i];
		__properties[i] = nil;
	end;

	for i = 1, #__methods do 
		__methods[__methods[i].Name] = __methods[i];
		__methods[i] = nil;
	end;
	
	local PublicClassPattern, PrivateClassPattern, InnerEnvMeta, OldInnerEnv = ConstructClass(__fields, __properties, __methods, Envir, RawClass);

	return PublicClassPattern;
end;

HELLua_Class.Class = {};

function HELLua_Class.Class.CreateClass(classCast, nameOfClass, parentClass)
		
	if (not (type(nameOfClass) == "string")) then 
		error(Errors.ENameIsInvalid);
	end;

	if (nameOfClass ~= 'TObject') and (parentClass == nil) then 
		parentClass = 'TObject';
	end;

	if (ClassesTree[nameOfClass] ~= nil) then 
		error(Errors.EClassNameDuplication:format(tostring(nameOfClass)));
	end;

	local ParentedTable = {};
	local InheritedFrom = false;
	if ((type(parentClass) == "string")) then 
		if (ClassesTree[parentClass] ~= nil) then 
			InheritedFrom = true;
			ParentedTable = ClassesTree[parentClass];
		else 
			error(Errors.EParentClassIsNotDefined:format(parentClass));
		end;
	end;

	local ParentFields = ((type(ParentedTable.__fields) == "table")) and (ParentedTable.__fields) or {};
	local ParentMethods = ((type(ParentedTable.__methods) == "table")) and ParentedTable.__methods or {};
	local ParentProperties = ((type(ParentedTable.__properties) == "table")) and ParentedTable.__properties or {};

	local function ExistsFunction(Name, List)
		local Result, Index, Obj = false, -1, nil;
		for i = 1, #List do 
			if List[i].Name == Name then 
				Result, Index, Obj = true, i, List[i];
				break;
			end;
		end;
		return Result, Index, Obj;
	end;

	local function PartExistsFunction(Name, Lists, Names)
		for i = 1, #Lists do 
			local Exists, Index, Obj = Lists[i](Name);
			if Exists then return Exists, Index, Names[i], Obj; end;
		end;
		return false, -1, nil, nil;
	end;

	local __properties = {};
	local __fields = {};
	local __methods = {};
	
	local function FieldExists(Name) return ExistsFunction(Name, __fields); end;
	local function PropertyExists(Name) return ExistsFunction(Name, __properties); end;
	local function MethodExists(Name) return ExistsFunction(Name, __methods); end;

	local function PartExists(Name)
		return PartExistsFunction(Name, {FieldExists, PropertyExists, MethodExists}, {Type_Field, Type_Property, Type_Method});
	end;
	
	local function PartExistsWithoutMethod(Name)
		return PartExistsFunction(Name, {FieldExists, PropertyExists}, {Type_Field, Type_Property});
	end;
	
	local function PartExistsWithoutProperty(Name)
		return PartExistsFunction(Name, {FieldExists, MethodExists}, {Type_Field, Type_Method});
	end;

	local function ParentFieldExists(Name) return ExistsFunction(Name, ParentFields); end;
	local function ParentMethodExists(Name) return ExistsFunction(Name, ParentMethods); end;
	local function ParentPropertyExists(Name) return ExistsFunction(Name, ParentProperties); end;
	local function ParentPartExists(Name) 
		return PartExistsFunction(Name, {ParentFieldExists, ParentPropertyExists, ParentMethodExists}, {Type_Field, Type_Property, Type_Method});
	end;
	local function ParentPartExistsWithoutProperty(Name)
		return PartExistsFunction(Name, {ParentFieldExists, ParentMethodExists}, {Type_Field, Type_Method});
	end;

	local Native, NativeObj = false, nil;

	-- All scopes are enumerable.
	for i = 1, #classCast do 
		local Scope = classCast[i];
		if (not (type(Scope) == "table")) then error(Errors.EUnexpectedType:format(type(Scope), 'table')); end;
		for k = 1, #Scope do 
			local Obj = Scope[k];
			if PartExists(Obj.Name) then 
				error(Errors.ENameDuplication:format(tostring(Obj.Name)));
			end;
			local ParentExists, ParentIndex, ParentType, PObj = ParentPartExists(Obj.Name);
			if (ParentExists) and (Obj.Type ~= ParentType) then
				error(Errors.EPartTypeCannotBeOverriden:format(tostring(Obj.Name), tostring(ParentType), tostring(Obj.Type)));
			end;
			if (ParentExists) and (PObj.Enclosed) then 
				error(Errors.EEnclosedPartCannotBeOverriden:format(tostring(Obj.Name)));
			end;
			if Obj.Type == Type_Field then 
				table.insert(__fields, Obj);
			elseif Obj.Type == Type_Property then	
				local Write = Obj.Write;
				local Read = Obj.Read;
				if ((type(Write) == "string")) then 
					local Ex, ID, Type, PObj = PartExistsWithoutProperty(Write);
					if not Ex then 
						Ex, ID, Type, PObj = ParentPartExistsWithoutProperty(Write);
						if not Ex then 
							error(Errors.EPropertySetterIsInvalid:format(tostring(Write))); 
						elseif (Type == Type_Field) and (PObj.Constant) then 
							error(Errors.EPropertyCannotWriteToConstantField:format(tostring(Obj.Name), tostring(PObj.Name)));
						end;
					elseif (Type == Type_Field) and (PObj.Constant) then
						error(Errors.EPropertyCannotWriteToConstantField:format(tostring(Obj.Name), tostring(PObj.Name)));
					end;
					if (Obj.Native) then 
						if Native then 
							error(Errors.ENativePropertyCanBeOnlyOne:format(tostring(Obj.Name), tostring(NativeObj.Name)));
						end;
						NativeObj = Obj;
						Native = true;
					end;
				end;
				if ((type(Read) == "string")) then 
					local Ex, ID, Type = PartExistsWithoutProperty(Read);
					if not Ex then 
						local Ex, ID, Type = ParentPartExistsWithoutProperty(Read);
						if not Ex then 
							error(Errors.EPropertyGetterIsInvalid:format(tostring(Read))); 
						end;
					end;
				end;
				table.insert(__properties, Obj);
			elseif Obj.Type == Type_Method then
				if PartExists(Obj.Name) then 
					error(Errors.ENameDuplication:format(tostring(Obj.Name)));
				end;
				if Obj.Linked ~= nil then
					local Ex, Index = MethodExists(Obj.Linked);
					if not Ex then 
						error(Errors.ELinkedMethodHasNoEndLink:format(tostring(Obj.Name), tostring(Obj.Linked)));
					end;
					if not HELLua_Class.Class.MethodsEqual(__methods[Index], Obj) then 
						error(Errors.ELinkedMethodHasInvalidDefinition:format(tostring(Obj.Name), tostring(Obj.Linked)));
					end;
					if __methods[Index].Linked then 
						error(Errors.ELinkedMethodLinkingPointer:format(tostring(Obj.Name), tostring(Obj.Linked)));
					end;
					local Lnks = __methods[Index].Links;
					if (not (type(Lnks) == "table")) then 
						__methods[Index].Links = {};
						Lnks = __methods[Index].Links;
					end;
					table.insert(Lnks, Obj);
				end; 
				table.insert(__methods, Obj);
			else 
				error(Errors.EUnknownType:format(tostring(Obj.Type)));
			end;
		end;
	end;

	if not Native then 
		for k,v in pairs(__properties) do 
			if v.Native then 
				Native = true;
				NativeObj = v;
				break;
			end;
		end;
	end;

	if InheritedFrom then 
		__fields = HL.CollideTablesByPropertySafe(ParentedTable.__fields, __fields, 'Name');
		__properties = HL.CollideTablesByPropertySafe(ParentedTable.__properties, __properties, 'Name');
		__methods = HL.CollideTablesByPropertySafe(ParentedTable.__methods, __methods, 'Name'); 
	end;

	local RawClass = {
		__properties = __properties;
		__fields = __fields;
		__methods = __methods;
		__name = nameOfClass;
		__parentPointer =  (InheritedFrom and ParentedTable or nil);
	};

	ClassesTree[nameOfClass] = RawClass;

	local Implementation = {};
	-- local ImplementationMeta, OldEnv = HL.CreateWithEnvironment(3);

	-- Implementation = setmetatable(Implementation, ImplementationMeta);
	
	if ((type(classCast.Implementation) == "function")) then 
		-- Implementation
		with(getfenv(2), function() 
			with(Implementation, classCast.Implementation);
		end);
	end;

	-- Implementation = setmetatable(Implementation, {});
	
	local DefinedMethods = HL.DeepCopy(__methods);

	for k,v in pairs(Implementation) do
		local Found, Index = MethodExists(k);
		if Found then 
			if (__methods[Index].Linked ~= nil) then 
				error(Errors.ELinkedMethodCannotHaveImplementation:format(tostring(k)));
			end;
			if ((type(v) == "function")) then 
				DefinedMethods[Index] = nil;
				__methods[Index].Function = v;
				__methods[Index].Abstract = false;
				__methods[Index].Owner = RawClass;
				if ((type(__methods[Index].Links) == "table")) then 
					for iter = 1, (#__methods[Index].Links) do 
						__methods[Index].Links[iter].Function = v;
						__methods[Index].Links[iter].Abstract = false;
						__methods[Index].Links[iter].Owner = RawClass;
					end;
				end;
			else 
				error(Errors.EMethodTypeIsViolated:format(tostring(k), type(v)));
			end;
		else 
			Found, Index = FieldExists(k);
			if Found then 
				if __fields[Index].Constant then 
					error(Errors.ECannotChangeConstantField:format(tostring(k)));
				end;
				__fields[Index].Content = v;
			else
				if ((type(v) == "function")) then 
					error(Errors.EImplementedMethodIsNotDefined:format(tostring(k)));
				else 
					error(Errors.EImplementedFieldHasNoDefinition:format(tostring(k)));
				end; 
			end;
		end;
	end;

	for k,v in pairs(DefinedMethods) do 
		if ((type(v) == "table")) then 
			if ((type(v.Function) == "function")) or (v.Linked) or (v.Abstract) then 
				-- print('++$ Cleared ' + k); 
				DefinedMethods[k] = nil;
			end;
		end;
	end;

	if not HL.TableIsEmpty(DefinedMethods) then 
		local StrMethods = HL.ConcatByProperty(DefinedMethods, 'Name', ',');
		StrMethods = '(' + StrMethods + ')';
		error(Errors.EMethodDefinedIsNotImplemented:format(StrMethods));
	end;

	local Operators = {};

	for k,v in pairs(__methods) do 
		if (v.Following) or (v.Virtual) then 
			v.Owner = RawClass;
		elseif (v.Operator) then 
			Operators[v.Name:lower()] = v;
		end;
	end;

	--[[
		ClassConstructionMethod is the only right constructor.
	]]

	local StaticClassMethods = {};
	local ConstructorsCount = 0;

	for i, meth in pairs(__methods) do 
		if (__methods[i].Static) and (__methods[i].Visibility == Visibility_Public) then 
			-- table.insert(StaticClassMethods, __methods[i]);
			StaticClassMethods[meth.Name] = __methods[i];
		elseif (__methods[i].Constructor) then 
			-- table.insert(StaticClassMethods, __methods[i]);
			StaticClassMethods[meth.Name] = __methods[i];
			(function() local A = ConstructorsCount; ConstructorsCount = ConstructorsCount + 1; return A; end)();
		end;
	end;

	if ConstructorsCount <= 0 then 
		error(Errors.ENoConstructorsAreDefined);
	end;

	local EnvirMeta, OldEnvir = HL.CreateWithEnvironment(3);
	local Envir = { InnerEnvMeta = EnvirMeta; OldInnerEnv = OldEnvir; };

	-- HL.PrintTable(getfenv(2));

	-- HL.PrintTable(OldEnvir.__ENV);
	-- print(OldEnvir.TObject);

	function StaticClassIndexer(self, index)
		local i = index;
		if (StaticClassMethods[i] ~= nil) then
			if not StaticClassMethods[i].Constructor then 
				local StaticMethod = StaticClassMethods[i];
				return HL.CreateCallableTable(function(self, ...) return StaticMethod.Function(...); end);
			else 
				local ConstructorMethod = StaticClassMethods[i];
				return HL.CreateCallableTable(function(self, ...)
					local PClass = ClassConstructionMethod(__fields, __properties, __methods, Envir, RawClass);
					PClass[index](...);
					return PClass;
				end);
			end;
		end; 
	end;

	local ClassPatternForPublicUsage = setmetatable({}, 
		{ 	__index = StaticClassIndexer; 
			__newindex = function() error('Cannot assign values to static classes.'); end });

	-- return ClassConstructionMethod;
	return ClassPatternForPublicUsage;
end;

with(HELLua_Class, function() 

	-- Class = {};
	
	with(Class, function() 
		
		--[[ Visibility ]]
		
		function ReloadClassTree()
			ClassesTree = {};
		end;

		function Public(objects)
			if not ((type(objects) == "table")) then 
				error(Errors.EUnexpectedType:format(type(objects), 'table'));
			end;
			for ind, object in pairs(objects) do 
				object.Visibility = Visibility_Public;
			end;
			return objects;
		end;
		
		function Protected(objects)
			if not ((type(objects) == "table")) then 
				error(Errors.EUnexpectedType:format(type(objects), 'table'));
			end;
			for ind, object in pairs(objects) do 
				object.Visibility = Visibility_Private;
			end;
			return objects;
		end;
		
		function Private(objects)
			if not ((type(objects) == "table")) then 
				error(Errors.EUnexpectedType:format(type(objects), 'table'));
			end;
			for ind, object in pairs(objects) do 
				if (object.Type == Type_Method) and (object.Constructor) then 
					error(Errors.EConstructorCannotBePrivate:format(tostring(object.Name)));
				elseif (object.Type == Type_Method) and (object.Operator) then 
					error(Errors.EOperatorMustBePublic:format(tostring(object.Name)));
				end;
				object.Visibility = Visibility_Private;
			end;
			return objects;
		end;
		
		--[[ Miscs ]]
		
		function Field(FieldObj)
			local name = FieldObj.Name;
			local Constant = FieldObj.Constant;
			local Enclosed = FieldObj.Enclosed;
			if (not (type(name) == "string")) then error(Errors.ENameIsInvalid); end;
			if (not (type(Constant) == "boolean")) then Constant = false; end;
			if (not (type(Enclosed) == "boolean")) then Enclosed = false; end;
			if (Constant and (FieldObj.Content == nil)) then 
				error(Errors.EConstantFieldMustHaveValue:format(tostring(name)));
			end;
			return 
				{ Type = Type_Field; 
					Name = name;
					Visibility = Visibility_Public;
					Content = FieldObj.Content;
					Constant = Constant; 
					Enclosed = Enclosed; };
		end;
		
		function Property(PropObj)
			if not ((type(PropObj) == "table")) then 
				error(Errors.EUnexpectedType:format(type(PropObj), 'table'));
			end;
			
			local PName = PropObj.Name;
			local PWrite = (((type(PropObj.Write) == "string")) and PropObj.Write or nil);
			local PRead = (((type(PropObj.Read) == "string")) and PropObj.Read or nil);
			local Enclosed = PropObj.Enclosed;
			local Indexed = PropObj.Indexed;
			local Native = PropObj.Native;
			if (not (type(Enclosed) == "boolean")) then Enclosed = false; end;
			if (not (type(Indexed) == "boolean")) then Indexed = false; end;
			if (not (type(Native) == "boolean")) then Native = false; end;
			
			if not ((type(PName) == "string")) then error(Errors.EUnexpectedType:format(type(PName), 'string')); end;
			if not (PRead or PWrite) then error(Errors.EPropertyIsUnreachable); end;
			if (Native and not Indexed) then error(Errors.ENativePropertyMustBeIndexed:format(tostring(Name))); end;
			
			return 
				{	Type = Type_Property;
					Name = PName;
					Visibility = Visibility_Public;
					Read = PRead;
					Write = PWrite;
					Enclosed = Enclosed;
					Native = Native;
					Indexed = Indexed; };
		end;
		
		function Method(MethodObj)
			local Name = MethodObj.Name;
			local Static = MethodObj.Static;
			local Constructor = MethodObj.Constructor;
			local Virtual = MethodObj.Virtual;
			local Enclosed = MethodObj.Enclosed;
			local Abstract = MethodObj.Abstract;
			local Operator = MethodObj.Operator;
			if (not (type(Name) == "string")) then error(Error.ENameIsInvalid); end;
			if (not (type(Static) == "boolean")) then Static = false; end;
			if (not (type(Constructor) == "boolean")) then Constructor = false; end;
			if (not (type(Virtual) == "boolean")) then Virtual = false; end;
			if (not (type(Enclosed) == "boolean")) then Enclosed = false; end;
			if (not (type(Abstract) == "boolean")) then Abstract = false; end;
			if (not (type(Operator) == "boolean")) then Operator = false; end;

			if Static and Constructor then error(Errors.EConstructorCannotBeStatic:format(Name)); end;
			if Static and Virtual then error(Errors.EStaticMethodCannotBeVirtual:format(Name)); end;
			if Abstract and not Virtual then error(Errors.EAbstractMethodMustBeVirtual:format(Name)); end;
			if Constructor and Abstract then error(Errors.EConstructorCannotBeAbstract:format(Name)); end;
			if Constructor and Enclosed then error(Errors.EConstructorCannotBeEnclosed:format(Name)); end;
			if Operator and (Constructor or Virtual or Abstract or Enclosed) then error(Errors.EOperatorCannotBeModified:format(Name)); end;
			if Operator and not Static then error(Errors.EOperatorMustBeStatic:format(Name)); end;

			return 
				{	Type = Type_Method;
					Name = Name;
					Static = Static;
					Constructor = Constructor;
					Linked = (((type((MethodObj.Linked)) == "string")) and MethodObj.Linked or nil);
					Enclosed = Enclosed;
					Abstract = Abstract;
					Operator = Operator;
					Virtual = Virtual };
		end;

		function MethodsEqual(MethodFirst, MethodSecond)
			local M = MethodFirst;
			local N = MethodSecond;
			return 
				(M.Static == N.Static) and 
				(M.Constructor == N.Constructor) and 
				(M.Virtual == N.Virtual) and 
				(M.Abstract == N.Abstract) and 
				(M.Enclosed == N.Enclosed);
		end;
		
		--[[ Main ]]

		function ArrayToTable(Array, Prop)
			for i = 1, #Array do 
				Array[Array[i][Prop]] = Array[i];
				Array[i] = nil;
			end;
			return Array;
		end;

	end);

end);

return HELLua_Class;
