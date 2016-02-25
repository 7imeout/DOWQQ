import std.stdio, std.regex, std.conv;

interface ExprC {}

class numC : ExprC {
   int n;
   this(int n) {
      this.n = n;
   }
}
class boolC : ExprC {
   bool b;
   this(bool b) {
      this.b = b;
   }
}
class idC : ExprC {
   string s;
   this(string s) {
      this.s = s;
   }
}
class appC : ExprC {
   ExprC fun; 
   ExprC[] args;
   this(ExprC fun, ExprC[] args) {
      this.fun = fun; 
      this.args = args;
   }
}
class BinopC : ExprC {
   string op; 
   ExprC lft; 
   ExprC rht;
   this(string op, ExprC lft, ExprC rht) {
      this.op = op; 
      this.lft = lft; 
      this.rht = rht;
   }
}
class ifC : ExprC {
   ExprC tst; 
   ExprC thn; 
   ExprC els;
   this(ExprC tst, ExprC thn, ExprC els) {
      this.tst = tst; 
      this.thn = thn; 
      this.els = els;
   }
}
class lamC : ExprC {
   string[] args; 
   ExprC bdy; 
   this(string[] args, ExprC bdy) {
      this.args = args; 
      this.bdy = bdy;
   }
}

class Binding {
   string symbol; 
   Value val;
   this(string symbol, Value val) {
      this.symbol = symbol;
      this.val = val;
   }
}
alias Env = Binding[];
alias MtEnv = Binding[0];

interface Value {}

class numV : Value {
   int n;
   this(int n) {
      this.n = n;
   }
}
class boolV : Value {
   bool b;
   this(bool b) {
      this.b = b;
   }
}
class closV : Value {
   string[] args; 
   ExprC bdy; 
   Env env;
   this(string[] args, ExprC bdy, Env env) {
      this.args = args;
      this.bdy = bdy;
      this.env = env;
   }
}

void main() {
   writeln("DOWQQ");
   
   // tests for parse
   assert((cast(numC)parse("0")).n == 0, "failed to parse \"0\"");
   assert((cast(numC)parse("100")).n == 100, "failed to parse \"100\"");
   assert((cast(numC)parse("-99")).n == -99,  "failed to parse \"-99\"");
   assert((cast(numC)parse("-0")).n == 0, "failed to parse \"-0\"");
   assert((cast(boolC)parse("true")).b == true, "failed to parse \"true\"");
   assert((cast(boolC)parse("false")).b == false, "failed to parse \"false\"");

   writeln("All asserts passed");
}

string topEval(string s) {
   return "";
}

string serialize(Value val) {
   return "";
}

Value interp(ExprC e, Env env) {
   if (cast(numC)e) {
      return new numV((cast(numC)e).n);
   }
   else if (cast(boolC)e) {
      return new boolV((cast(boolC)e).b);
   }

   return new numV(1);
}

ExprC parse (string s) {
   if (matchFirst(s, r"(-?[1-9])+|(-?0)")) {
      return new numC(to!int(s));
   }
   if (matchFirst(s, r"true")) {
      return new boolC(true);
   }
   if (matchFirst(s, r"false")) {
      return new boolC(false);
   }
   if (matchFirst(s,))

   assert(false, "Invalid OWQQ expression: \"" ~ s ~ "\"");
   return new numC(0);
}

