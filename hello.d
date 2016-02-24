import std.stdio;

void main() {
   writeln("Hello World!");
   int x = 1;
   int* xp = &x;
   int* p = xp;// &x;
   writeln(xp);
}
