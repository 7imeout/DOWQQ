import std.stdio;
import std.regex;
import std.conv;
import std.exception;
import std.format;


//////////////////////
// Data Definitions //
//////////////////////

/** 
 * ExprC (core language) is either:
 * - a numC (number),
 * - a boolC (boolean)
 * - an idC (identifier),
 * - an appC (function application),
 * - a binopC (binary operation),
 * - an ifC (conditional), or
 * - a lamC (anonymous function)
 */
interface ExprC {}

/* numC */
class numC : ExprC {
   int n;
   this(int n) {
      this.n = n;
   }
}

/* boolC */
class boolC : ExprC {
   bool b;
   this(bool b) {
      this.b = b;
   }
}

/* idC */
class idC : ExprC {
   string s;
   this(string s) {
      this.s = s;
   }
}

/* appC */
class appC : ExprC {
   ExprC fun; 
   ExprC[] args;
   this(ExprC fun, ExprC[] args) {
      this.fun = fun; 
      this.args = args;
   }
}

/* binopC */
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

/* ifC */
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

/* lamC */
class lamC : ExprC {
   string[] args; 
   ExprC bdy; 
   this(string[] args, ExprC bdy) {
      this.args = args; 
      this.bdy = bdy;
   }
}

/**
 * Binding is:
 * - a bind from symbol (string) to a value (Value)
 */
class Binding {
   string symbol; 
   Value val;
   this(string symbol, Value val) {
      this.symbol = symbol;
      this.val = val;
   }
}

/* An environment is an array of bindings */
alias Env = Binding[];

/* An empty environment is an empty array */
alias MtEnv = Binding[0];

/**
 * A value is:
 * - a number,
 * - a boolean, or
 * - a closure (with arguments, body, and an Env)
 */
interface Value {}

/* number */
class numV : Value {
   int n;
   this(int n) {
      this.n = n;
   }
}

/* boolean */
class boolV : Value {
   bool b;
   this(bool b) {
      this.b = b;
   }
}

/* closure */
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


/////////////////////
// Main Test Cases //
/////////////////////

/**
 * Serves as a test runner for DOWQQ
 * @return {void} nothing
 */
void main() {
   writeln("\nAssignment 6: DOWQQ (OWQQ in D)");
   writeln("Corbin G | Annie L | Mike R | Greg S");
   writeln("\nRunning tests ...");

   MtEnv mt;
   numV numR;
   boolV boolR;

   numR = cast(numV)interp(new binopC("+", new binopC("*", new numC(5), new numC(3)), new numC(4)), mt);
   assert(numR.n == 19);
   boolR = cast(boolV)interp(new ifC(new binopC("eq?", new numC(5), new numC(5)), new boolC(true), new boolC(false)), mt);
   assert(boolR.b == true);

   // tests for parse
   // assert((cast(numC)parse("0")).n == 0, "failed to parse \"0\"");
   // assert((cast(numC)parse("100")).n == 100, "failed to parse \"100\"");
   // assert((cast(numC)parse("-99")).n == -99,  "failed to parse \"-99\"");
   // assert((cast(numC)parse("-0")).n == 0, "failed to parse \"-0\"");
   // assert((cast(boolC)parse("true")).b == true, "failed to parse \"true\"");
   // assert((cast(boolC)parse("false")).b == false, "failed to parse \"false\"");

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

   numR = cast(numV)interp(new appC(new lamC(["x","y"], new binopC("+", new idC("x"), new idC("y"))), [new numC(5), new numC(2)]), mt);
   assert(numR.n == 7, format("got: %s", numR.n));

   boolR = cast(boolV)interp(new ifC(new binopC("eq?", new numC(6), new numC(5)), new boolC(true), new boolC(false)),  mt);
   assert(boolR.b == false, format("got: %s", boolR.b));

   boolR = cast(boolV)interp(new ifC(new binopC("eq?", new numC(5), new numC(5)), new boolC(true), new boolC(false)), mt);
   assert(boolR.b == true, format("got: %s", boolR.b));

   assertThrown(interp(new binopC("/", new numC(1), new numC(0)), mt));
   assertThrown(interp(new binopC("boo", new boolC(false), new boolC(false)), mt));
   assertThrown(interp(new binopC("+", new boolC(false), new numC(3)), mt));

   writeln("All asserts true!\n");
}


/////////////////////////
// Interface Functions //
/////////////////////////

/**
 * Consumes a string equivalent to a surface syntax, and evaluates the result.
 * @param  {string} string s surface syntax of DOWQQ
 * @return {string} evaluation result
 */
string topEval(string s) {
   return "";
}

/**
 * Consumes a Value and serializes it to a string.
 * @param  {Value} Value val resultant value from interp
 * @return {string} string serializes result of the value
 */
string serialize(Value val) {
    if (cast(numV)val) {
        return to!string((cast(numV)val).n);
    }
    if (cast(boolV)val) {
        return to!string((cast(boolV)val).b);
    }
    if (cast(closV)val) {
        return "#<procedure>";
    } 

    throw new Exception("Attempt to serialize unknown value"); 
}

/**
 * Evaluates the DOWQQ AST into a Value.
 * @param  {ExprC} ExprC e DOWQQ AST
 * @param  {Env} Env env current environment
 * @return {Value}  result of the evaluation
 */
Value interp(ExprC e, Env env) {
   if (cast(numC)e) {
      return new numV((cast(numC)e).n);
   }
   else if (cast(boolC)e) {
      return new boolV((cast(boolC)e).b);
   }
   else if (cast(binopC)e) {
      binopC binopExprC = cast(binopC)e;
      return binop(binopExprC.op, 
                   interp(binopExprC.lft, env), 
                   interp(binopExprC.rht, env));
   }
   else if (cast(idC)e) {
       return lookup((cast(idC)e).s, env);
   }
   else if (cast(ifC)e) {
      ifC ifExprC = cast(ifC)e;
      return ifcond(interp(ifExprC.tst, env), 
                    ifExprC.thn, 
                    ifExprC.els,
                    env);
   }   
   else if(cast(lamC)e) {
      return new closV((cast(lamC) e).args , (cast(lamC) e).bdy, env);
   }
   else if(cast(appC)e) {
       auto app = to!appC(e); 
       
       Value clV = interp(app.fun, env);
       
       if (cast(closV)clV) {
           
           auto clv = to!closV(clV);
        
           if (clv.args.length == app.args.length) {
               Env extndEnv = new Env(clv.args.length);
               
               for (int i = 0; i < clv.args.length; i++) {
                   extndEnv[i] = new Binding(clv.args[i], interp(app.args[i], env));
               }
               
               // Concatenate 
               extndEnv ~= env;
               
               return interp(clv.bdy, extndEnv);                      
           } 
           else {
               throw new Exception ("function arg length != num args supplied");
           }
       } 
       else {
           throw new Exception("Improperly formed function");
       }
   }
   else {
      throw new Exception(format("Invalid AST: %s", e));
   }
}

/**
 * Parses the surface syntax into DOWQQ AST.
 * @param  {string} string s surface syntax of OWQQ
 * @return {ExprC} AST of DOWQQ
 */
ExprC parse (string s) {
      return new numC(0);
   if (matchFirst(s, r"(-?[1-9])+|(-?0)")) {
      return new numC(to!int(s));
   }
   if (matchFirst(s, r"true")) {
      return new boolC(true);
   }
   if (matchFirst(s, r"false")) {
      return new boolC(false);
   }
   throw new Exception("Invalid OWQQ expression: \"" ~ s ~ "\"");
}


///////////////////////
// Helper Functions  //
///////////////////////

/**
 * Consumes operator and two values to perform a binary operation.
 */
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

/**
 * Performs eq? on non-number values.
 */
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

/**
 * Checks of an operator is a binary comparison operator.
 */
bool isBinopCompOp(string op) {
   return matchFirst(op, r"eq\?") || matchFirst(op, r"<=");
}

/**
 * Performs binary comparison (eq? or less-than-equal-to).
 */
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

/**
 * Checks if an operator is a binary arithmetic operator.
 */
bool isBinopArithOp(string op) {
   return (matchFirst(op, r"\+") || matchFirst(op, r"-")
        || matchFirst(op, r"\*") || matchFirst(op, r"/"));
}

/**
 * Performs a binary arithmetic operation on two values.
 */
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

/**
 * Evaluates if condition and result.
 */
Value ifcond(Value tst, ExprC thn, ExprC els, Env env) {
   Value result;

   if (cast(boolV)tst)
      if ((cast(boolV)tst).b)
         result = interp(thn, env);
      else
         result = interp(els, env);
   else
      throw new Exception(
         "if's condition did not evaluate to a boolean value");

   return result;
}


/**
 * Looks for value tied to symbol in the environment
 * @param  {String} symbol to look up in environment
 * @param  {Env} Env environment with pairings of symbol to Value
 * @return {Value} Found pairing of value to symbol or error
 */
Value lookup(string symbol, Env env) {

    for (int i = 0; i < env.length; i++) {
        if (symbol == env[i].symbol) {
            return env[i].val;
        }
    }

    throw new Exception(format("Lookup failed to find symbol: %s", symbol));    
}
