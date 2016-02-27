import std.stdio;
import std.regex;
import std.conv;
import std.exception;
import std.format;

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
class binopC : ExprC {
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

class boolIfException : Exception{
   this(string msg){
      super(msg);
   }

}

void main() {
   writeln("DOWQQ");
   MtEnv s;
   Value v = interp(new BinopC("+", new BinopC("*", new NumC(5), new NumC(3)), new NumC(4)));
   assert((cast(numV) v).n == 19);
   v = interp(new ifC(new BinopC("eq?", new numC(5), new numC(5)), new boolC(true), new boolC(false)));
   assert((cast(boolV) v).b == true);

   // tests for parse
   // assert((cast(numC)parse("0")).n == 0, "failed to parse \"0\"");
   // assert((cast(numC)parse("100")).n == 100, "failed to parse \"100\"");
   // assert((cast(numC)parse("-99")).n == -99,  "failed to parse \"-99\"");
   // assert((cast(numC)parse("-0")).n == 0, "failed to parse \"-0\"");
   // assert((cast(boolC)parse("true")).b == true, "failed to parse \"true\"");
   // assert((cast(boolC)parse("false")).b == false, "failed to parse \"false\"");

   MtEnv mt;
   numV numR;
   boolV boolR;

   numR = cast(numV)interp(new binopC("+", new numC(1), new numC(2)), mt);
   assert(numR.n == 3);

   numR = cast(numV)interp(new binopC("-", new numC(2), new numC(1)), mt);
   assert(numR.n == 1);

   numR = cast(numV)interp(new binopC("*", new numC(2), new binopC("+", new numC(1), new numC(2))), mt);
   assert(numR.n == 6);

   numR = cast(numV)interp(new binopC("/", new binopC("+", new numC(1), new numC(2)), new binopC("+", new numC(1), new numC(2))), mt);
   assert(numR.n == 1);

   boolR = cast(boolV)interp(new binopC("eq?", new numC(2), new numC(2)), mt);
   assert(boolR.b == true, format("got: %s", boolR.b));

   boolR = cast(boolV)interp(new binopC("eq?", new numC(2), new numC(-2)), mt);
   assert(boolR.b == false, format("got: %s", boolR.b));

   boolR = cast(boolV)interp(new binopC("<=?", new numC(-2), new numC(2)), mt);
   assert(boolR.b == true, format("got: %s", boolR.b));

   boolR = cast(boolV)interp(new binopC("eq?", new numC(2), new numC(-2)), mt);
   assert(boolR.b == false, format("got: %s", boolR.b));

   boolR = cast(boolV)interp(new binopC("eq?", new numC(2), new boolC(false)), mt);
   assert(boolR.b == false, format("got: %s", boolR.b));

   boolR = cast(boolV)interp(new binopC("eq?", new boolC(false), new boolC(false)), mt);
   assert(boolR.b == true, format("got: %s", boolR.b));

   assertThrown(interp(new binopC("/", new numC(1), new numC(0)), mt));
   assertThrown(interp(new binopC("boo", new boolC(false), new boolC(false)), mt));
   assertThrown(interp(new binopC("+", new boolC(false), new numC(3)), mt));

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
   else if(cast(ifC) e){
      if(cast(boolC)(cast(ifC)e).tst){
         if((cast(boolV)interp((cast(ifC)e).tst, env)).b){
            interp((cast(ifC)e).thn, env);
         }
         else{
            interp((cast(ifC)e).els, env);
         }
      }
      else{
         throw new boolIfException("First clause of an if statement must equate to a boolean");
      }
   }
   else if(cast(lamC) e){
      return new closV((cast(lamC) e).args , (cast(lamC) e).bdy, env);
   else if (cast(boolC)e) {
      return new boolV((cast(boolC)e).b);
   }
   else if (cast(binopC)e) {
      binopC binopExprC = cast(binopC)e;
      return binop(binopExprC.op, 
                   interp(binopExprC.lft, env), 
                   interp(binopExprC.rht, env));
   }

   return new numV(1);
}

Value binop(string op, Value lft, Value rht) {
   if (matchFirst(op, "eq?") && (!(cast(numV)lft) || !(cast(numV)rht)))
      return nonnumEq(lft, rht);
   else if (isBinopCompOp(op) && cast(numV)lft && cast(numV)rht)
      return binopComp(op, lft, rht);
   else if (matchFirst(op, "/") && ((cast(numV)rht).n == 0))
      throw new Exception("/: division by zero (DOWQQ)");
   else if (isBinopArithOp(op) && cast(numV)lft && cast(numV)rht)
      return binopArith(op, lft, rht);
   else if (!isBinopArithOp(op))
      throw new Exception(format("%s: not a binop", op));
   else
      throw new Exception("binop: one or more argument was not a number");
}

boolV nonnumEq(Value lft, Value rht) {
   bool result = false;

   if (cast(closV)lft || cast(closV)rht)
      result = false;
   else if (cast(boolV)lft && cast(boolV)rht) 
      result = ((cast(boolV)lft).b == (cast(boolV)rht).b);
   else
      result = false;

   return new boolV(result);
}

bool isBinopCompOp(string op) {
   return matchFirst(op, r"eq\?") || matchFirst(op, r"<=");
}

boolV binopComp(string op, Value lft, Value rht) {
   int l = (cast(numV)lft).n;
   int r = (cast(numV)rht).n;
   bool result = false;

   if (matchFirst(op, r"eq\?"))
      result = l == r;
   else if (matchFirst(op, r"<="))
      result = l <= r;
   else
      throw new Exception("%s invalid binop comparison operator", op);

   return new boolV(result);
}

bool isBinopArithOp(string op) {
   return (matchFirst(op, r"\+") || matchFirst(op, r"-")
        || matchFirst(op, r"\*") || matchFirst(op, r"/"));
}

numV binopArith(string op, Value lft, Value rht) {
   int l = (cast(numV)lft).n;
   int r = (cast(numV)rht).n;
   int result = 0;

   if (matchFirst(op, r"\+"))
      result = l + r;
   else if (matchFirst(op, r"-"))
      result = l - r;
   else if (matchFirst(op, r"\*"))
      result = l * r;
   else if (matchFirst(op, r"/"))
      result = l / r; 

   return new numV(result);
}

//ExprC parse (string s) {
//   if (matchFirst(s, r"(-?[1-9])+|(-?0)")) {
//      return new numC(to!int(s));
//   }
//   if (matchFirst(s, r"true")) {
//      return new boolC(true);
//   }
//   if (matchFirst(s, r"false")) {
//      return new boolC(false);
//   }
//   if (matchFirst(s,))

//   assert(false, "Invalid OWQQ expression: \"" ~ s ~ "\"");
//   return new numC(0);
//}

