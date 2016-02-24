import std.stdio;

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
   writeln("Assignment 3 in D");
   MtEnv s;
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

   return new numV(1);
}