import { init } from 'z3-solver';

export const newSolverImpl = (name) => {
  console.log("JS: Initing");
  return init()
    .then(({ Context }) => {
      console.log("JS: Returning context");
      return new Context(name);
    });
};

export const addVariablesImpl = (Z3, vs) => {
  let newVars = {};
  vs.forEach(({ typ, name }) => {
    let v = null;
    switch (typ) {
      case "int":
        v = Z3.Int.const(name);
        break;
      case "bool":
        v = Z3.Bool.const(name);
        break;
      default:
        throw Error("UNMATCHED: " + typ + " " + name);
    }
    newVars[name] = v;
  });
  return newVars;
};

export const solveImpl = (Z3, vs, expr) => {
  let solver = new Z3.Solver();
  let converted = convertExpr(Z3, vs, expr);
  // console.log(converted.toString());
  solver.add(converted);
  console.log(solver.toString());
  return solver.check()
    .then((checkResult) => {
      if (checkResult === "unsat") {
        throw Error("UNSAT!");
      } else {
        let model = solver.model();
        console.log("------------------------------------------------------------------------");
        let modelResults = {}
        model.decls().forEach(element => {
          let v = model.get(element);
          // TODO: the value function is only for Ints, but not bools?
          if (typeof v.value === 'function') {
            modelResults[element.name()] = Number(v.value());
            //console.log(element.name(), Number(v.value()));
          } else {
            // console.log("Variable " + element.name() + " doesn't have a value");
          }
        });
        return modelResults;
      }
    });
}

const convertExpr = (Z3, vs, e) => {
  // console.log(e);
  switch (e.op) {
    case "or":
      let newEs1 = e.l.map((clause) => convertExpr(Z3, vs, clause));
      return Z3.Or(...newEs1);
    case "and":
      let newEs2 = e.l.map((clause) => convertExpr(Z3, vs, clause));
      return Z3.And(...newEs2);
    case "implies":
      return Z3.Implies(convertExpr(Z3, vs, e.l), convertExpr(Z3, vs, e.r));
    case "iff":
      return Z3.Eq(vs[e.l], convertExpr(Z3, vs, e.r));
    case "eq":
      return vs[e.l].eq(e.r);
    case "ge":
      return vs[e.l].ge(e.r);
    case "lt":
      return vs[e.l].lt(e.r);
    case "var":
      return vs[e.l];
    case "tru":
      return true;

    default:
      throw Error("UNMATCHED Z3 EXPR");
  }
}
