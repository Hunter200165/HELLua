Sadly to say, this version of language is very obsolete and not used or developed anymore (except some cases). There is an another enhanced compiler of HELLua, but now it is not ready to face public. I am very sorry for any inconvenience created by that decision. If you are interested in getting new compiler or want to help developing it - feel free to join HFSoftware discord server, where you can either ask or talk to me (when I am not busy):  https://discord.gg/dDzK7vS

# ![](https://cdn.discordapp.com/emojis/490042668018040832.png?v=1) HELLua
HELLua - Hunter's Enhanced Lexeme Lua compiler - is new programming language based and written in pure Lua. Initially it was supposed to be a set of smart macros for Lua, but as number of macros grew up and new operators were added, this idea became programming language itself. 
HELLua has a compiler, written in pure Lua, that allows you to use macros in your code and very big amount of syntactic sugar. Firstly, all HELLua sources are compiled to Lua sources, and then it can be executed on every Lua interpreter.

HELLua compiler does not use functions, that are specific to version of interpreter. However, there are libraries (for example HELLua_ClassAPI), that use functions specific to `Lua 5.1` (such as `setfenv` and `getfenv`).

# HELLua Official Libraries
HELLua has a several official libraries, that can (or must) be used in HELLua projects.
There are:
  - `HELLua_API` - main and widely used library, that contains several important functions and features,
  - `HELLua_ClassAPI` - library, that allows programmer to use normal classes, requires `HELLua_API`,
  - `HELLua_ClassesLib` - library with already-written classes. Contains `TObject` implementation, therefore you cannot create new class without `HELLua_ClassesLib` being loaded. Requires `HELLua_API`, `HELLua_ClassAPI`;
