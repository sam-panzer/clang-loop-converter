// RUN: echo '[{"directory":".","command":"clang++ -c %T/test.cpp","file":"%T/test.cpp"}]' > %T/compile_commands.json
// RUN: cp "%s" "%T/test.cpp"
// RUN: remove-cstr-calls "%T" "%T/test.cpp"
// RUN: cat "%T/test.cpp" | FileCheck %s
// REQUIRES: shell
// FIXME: implement a mode for refactoring tools that takes input from stdin
// and writes output to stdout for easier testing of tools. 

namespace std {
template<typename T> class allocator {};
template<typename T> class char_traits {};
template<typename C, typename T, typename A> struct basic_string {
  basic_string();
  basic_string(const C *p, const A& a = A());
  const C *c_str() const;
};
typedef basic_string<char, std::char_traits<char>, std::allocator<char> > string;
}
namespace llvm { struct StringRef { StringRef(const char *p); }; }

void f1(const std::string &s) {
  f1(s.c_str());
  // CHECK: void f1
  // CHECK-NEXT: f1(s)
}
void f2(const llvm::StringRef r) {
  std::string s;
  f2(s.c_str());
  // CHECK: std::string s; 
  // CHECK-NEXT: f2(s)
}
void f3(const llvm::StringRef &r) {
  std::string s;
  f3(s.c_str());
  // CHECK: std::string s;
  // CHECK: f3(s)
}
