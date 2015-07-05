(** utilities package for polyhedra manipulation *)
BeginPackage["POLYUTILS`","MPPL`"];
(** usages *)

PUDualize::usage = " 
PUDualize[poly] dualizes the polyhedron and returns 
the handle to a new polyhedron.";

PUSubstitute::usage="
PUSubstitute[p,sublst] carries out a set of substitutions on
the constraint representation.";

PUIter::usage = " 
Carry out an iteration ";

FormExpLst::usage = "
FormExpLst[P,q,lambda,vars] forms the transformation that is required
to carry out the refinement operator";

ComputeTimeElapse::usage = "

";
ComputeTimeElapseLambdas::usage="

";

c0::usage = "
 The constant symbol ";




Begin["`private`"];

verb=False;

PPrint[x_] := If[ verb, Print[x]];

DualizeVar [ 
var_Symbol
] := Symbol[ "c"<> ToString[var] ]


(*ConstSymb = Symbol["c0"]; *)


MakeConstraintV [ 
  v : Vertex[coefflst_List,r_Integer],
  vars_List 
] :=
  With[
      { cexp = coefflst . vars }, 
      cexp + r * Symbol["c0"] >= 0
   ]

MakeConstraintR [ 
  v : Ray[coefflst_List],
  vars_List 
] :=
  With[
      { cexp = coefflst . vars }, 
      cexp  >= 0
   ]

MakeConstraintL[ 
  v : Line[coefflst_List],
  vars_List 
] :=
  With[
      { cexp = coefflst . vars }, 
      cexp  == 0
   ]


MakeConstraint[
  v_, 
  vars_List
] :=  Switch[v,
 _Vertex, MakeConstraintV[v,vars],
 _Ray, MakeConstraintR[v,vars],
 _Line,MakeConstraintL[v,vars],
 _, PPLError["Unknown head in generator"<>ToString[Head[v]] ]
];

  
PUDualize[
  poly: Poly[pid_Integer]
] := 
Module[ {vars,dvars,dim,gens,clst},
    vars = PPLVariables[poly];
    dvars = Map[ DualizeVar[#]&, vars];
    dim = MLPPLSpaceDimension[pid];
    gens = PPLGenerators[poly];
    If [dim == -1, 
        PPLError[" PUDualize: Bad PID:"<> ToString[pid]]
     ];
    clst = Map[ MakeConstraint[#,dvars]&, gens];
    PPLNew[clst,Join[dvars,{Symbol["c0"]}]]        
];


PUSubstitute[ 
  poly: Poly[pid_Integer],
  explst_List
] := 
  Module[{vars,clst,dlst,retp} ,
     vars = PPLVariables[poly];
     clst = PPLConstraints[poly];
     dlst = Map[ ExpandAll[#]&, (clst /. explst)] ;
     retp =PPLNew [dlst, vars];
    Switch[retp, 
    _Poly, retp,
    _, Print["Problem with PUSubstitute"]; 
       Print[dlst]; Print[retp];retp]

];


PUIter[
  poly: Poly[pid1_Integer],
  explst_List,
  inv:Poly[pid2_Integer]
] :=
Module[{diffp},
  diffp = PUSubstitute[poly,explst];
 
  PPLIntersect[diffp,poly];
  PPLUnionAndMinimize[diffp,inv];
  Switch[diffp, 
   _Poly, diffp,
   _, Print["Problem with PUITer."];diffp
]
];
   


FormExpLst [ 
  P_List,
  q_List,
  lambda: _Rational|_Integer,
  vars_List ] :=
  Module[ {dvars,rlst,cexp},
    dvars = Map[DualizeVar[#]&,vars];
    rlst = MapThread[  (#1 -> #2 + lambda * #1) &, {dvars, Transpose[P].dvars}];
    cexp = Symbol["c0"] -> lambda * Symbol["c0"] + q.dvars;
    Join[rlst,{cexp}]
];

V2Cons[
  v: Vertex[coeff_List,d_Integer],
  vars_List
]:= Module[{ duvar,vlist},
       vlist = Join[vars,{duvar}];
       cexp = coeff . vlist /. duvar -> 1;
       cexp >= 0
];

R2Cons[
  r: Ray[coeff_List],
  vars_List
  ]:= V2Cons[ Vertex[coeff,1],vars];

L2Cons[
  l: Line[coeff_List],
  vars_List
]:= 
  Module[{ duvar,vlist},
       vlist = Join[vars,{duvar}];
       cexp = coeff . vlist /. duvar -> 1;
       cexp >= 0
];


Gen2Cons[
  g : _Vertex|_Ray|_Line,
  vars_List
]:= Switch[g,
 _Vertex, V2Cons [g,vars],
 _Ray, R2Cons[g,vars],
 _Line, L2Cons [g,vars],
 _, PPLError["Unknown head in generator"<>ToString[Head[v]] ]
];


PUPrimalize[
  p: Poly[pid_Integer],
  vars_List
] := 
Module[ {gens,cons},
  gens=PPLGenerators[p];
  cons=Map[Gen2Cons[#,vars]&,gens];
  PPLNew[cons,vars]
];

  

ComputeTimeElapse[
  theta: Poly[p0_Integer],
  dynamics: {P_List, q_List},
  inv: Poly[p1_Integer],
  params: List[lambda:_Rational|_Integer, niters_Integer,nextrp_Integer,prec_Integer]
  ] :=  
 Module[
 {dinv,dth,explst,vars,dvars,pi,pi1,i,pi2,res},
    Print[lambda];
    dth = PUDualize[theta];
    dinv = PUDualize[inv];
    vars=PPLVariables[theta];
    dvars=PPLVariables[inv];
    explst = FormExpLst[P,q,lambda,dvars];
    pi=dth;
    For[i=0, i< niters, i++, 
      pi1 = PUIter[pi,explst,dinv];
      PPrint["Iteration "<>ToString[i]<>":"];
      PPrint[PPLConstraints[pi1]];
    (*  PPrint[PPLGenerators[pi1]]; *)
      If[ PPLContains[pi1,pi], 
        Print["Convergence detected"];
        Break[]];
      PPLDelete[pi];
      pi=pi1];
      PPrint["Starting Extrapolation Iteration"];
   For[i=0, i< nextrp, i++, 
      pi1=PUIter[pi,explst,dinv];
      pi2 = PPLExtrapolateNarrow[pi,pi1,prec];
      PPrint["Extrapolation "<>ToString[i]<>":"];
      PPrint[PPLConstraints[pi2]];
    (*  PPrint[PPLGenerators[pi2]] ;*)
      If[ PPLContains[pi1,pi], 
        PPrint["Convergence detected"];
        Break[]];
      PPLDelete[pi];
      PPLDelete[pi1];
      pi = pi2;
     ];
   PPrint["Starting Narrowing."];
   pi1 = PUIter[pi,explst,dinv];
   i=0;
   While[!PPLContains[pi1,pi],
      pi2=PPLStandardNarrow[pi,pi1];
      PPLDelete[pi];
      PPLDelete[pi1];
      pi = pi2;
      i=i+1;
      PPrint["Narrowing #"<>ToString[i]];
      PPrint[PPLConstraints[pi]];
   (*   PPrint[PPLGenerators[pi]];*)
      pi1 = PUIter[pi,explst,dinv];
    ];
    
    res=PUPrimalize[pi,vars];
    PPLIntersect[res,inv];
    PPrint["Final Result:"];
    PPrint[PPLConstraints[res]]; 
    (* PPrint[PPLGenerators[res]]; *)
    res
];

ComputeTimeElapseLambdas[
  theta: Poly[p0_Integer],
  dynamics: {P_List, q_List},
  inv: Poly[p1_Integer],
  pml: List[llist_List, niters_Integer,nextrp_Integer,prec_Integer]
  ] := 
Module[{resplst,vars},
    Print["Theta:"];
    Print[PPLConstraints[theta]];
    PPrint["inv:"];
    PPrint[PPLConstraints[inv]];
  resplst = Map[ ComputeTimeElapse[theta,dynamics,inv,
                              {#,niters,nextrp,prec}]&,
                 llist
                ];
  vars=PPLVariables[theta];
  rpoly=resplst[[1]];
  If[Length[resplst]>= 2,
    Map[PPLIntersectAndMinimize[rpoly,#]&,Rest[resplst]]
    ];  
  Print["Final Result:"];
  Print[PPLConstraints[rpoly]];
(*  PPrint[PPLGenerators[rpoly]];*)
  rpoly
];
 
                 
End[];
EndPackage[];




