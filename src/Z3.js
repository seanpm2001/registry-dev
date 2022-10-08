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
  vs.forEach(element => {
    let v = Z3.Int.const(element);
    newVars[element] = v;
  });
  return newVars;
};

export const solveImpl = (Z3, vs, expr) => {
  // return Z3.check()
  //   .then((checkResult) => {
  //     if (checkResult === "unsat") {
  //       throw Error("UNSAT!");
  //     } else {
  //       console.log("JS: Solving");

  //     }
  //   })
  return Z3.solve(convertExpr(Z3, vs, expr))
    .then((model) => {
      console.log("JS: Solved, getting model");
      let modelResults = {}
      let decls = model.decls()
      decls.forEach(element => {
        modelResults[element.name()] = Number(model.get(vs[element.name()]).value());
      });
      return modelResults;
    });
}

const convertExpr = (Z3, vs, e) => {
  switch (e.op) {
    case "or":
      let newEs1 = e.l.map((clause) => convertExpr(Z3, vs, clause));
      return Z3.Or(...newEs1);
    case "and":
      let newEs2 = e.l.map((clause) => convertExpr(Z3, vs, clause));
      return Z3.And(...newEs2);
    case "implies":
      return Z3.Implies(convertExpr(Z3, vs, e.l), convertExpr(Z3, vs, e.r));
    case "eq":
      return vs[e.l].eq(e.r);
    case "ge":
      return vs[e.l].ge(e.r);
    case "lt":
      return vs[e.l].lt(e.r);

    default:
      throw Error("UNMATCHED");
  }
}
