gglang
======

Good Game Language - A programming language designed for video games


Inspired by this youtube series: https://www.youtube.com/watch?v=TH9VCN6UkyQ, GGLang is an attempt to explore ideas that would make a good game programming language.


Current features include:
 inline llvm
 any order declaration

Planned features include:
 c foreign function interface
 incorperated build process
 pass/return by reference
 operator overloading
 algebraic types
 type inference
 default initialization
 named breaks
 support for vector types (ie: 4 x i32)
 
Possible advanced features include:
 reflection
 generics

Notable features not included in the language, 
  classes / inheritance
  traits / interfaces / typeclasses
  
The gg_compiler is implented as an LLVM frontend.  The current repository supports a Visual Studio 2012 (11.0) solution and includes the necessary windows compiled LLVM libraries.

twitter: @greysphere
