BeginPackage["MPPL`"];

(*** Usages ***)

Poly::usage = "Reference to PPL polyhedron.";

PPLBegin::usage = "PPLBegin[] initializes resources.";
PPLEnd::usage = "PPLEnd[] closes and destroys resources.";

PolyhedraVariables::usage = "
PolyhedronVariables[vars] sets the dimensions for PPL Calculator
functions (Polyhedron, Intersect, Hull, Project, PolyhedraApply).";

Polyhedron::usage = "
Polyhedron[constraints] constructs a polyhedron in the dimensions
specified by PolyhedronVariables.";

Intersect::usage = "
Intersect[p1, p2] sets p1 to the intersection of p1 and p2 and deletes
p2.  The polyhedra must have the dimensions specified by
PolyhedronVariables.";

Hull::usage = "
Hull[p1, p2] sets p1 to the convex hull of p1 and p2 and deletes p2.
The polyhedra must have the dimensions specified by
PolyhedronVariables.";

Project::usage = "
Project[vars, p] projects out vars from p.  p must have the dimensions
specified by PolyhedronVariables.  Project[vars, cond, p] projects out
vars from p, where vars satisfy cond.";

PolyhedraApply::usage = "
PolyhedraApply[fn, cts1, cts2] applies fn to the polyhedra formed from
the constraints cts1 and cts2 and returns the constraints.  The
polyhedra must have the dimensions specified by PolyhedronVariables.";

PPLNew::usage = "
PPLNew[cts, vars] returns the polyhedron formed by cts in dimensions
vars.";

PPLVariables::usage = "
PPLVariables[p] returns the dimension names of p.";

PPLCopy::usage = "
PPLCopy[p] returns a copy of p.";

PPLDelete::usage = "
PPLDelete[p] deletes the resources for p.";

PPLAddConstraints::usage = "
PPLAddConstraints[p, cts, [minimize]] adds the constraints cts to p.";

PPLConstraints::usage = "
PPLConstraints[p] returns the constraint representation of p.";

PPLVertices::usage = "
PPLVertices[p] returns the vertices of p.";

PPLGenerators::usage = "
PPLGenerators[p] returns the generators of p.";

PPLProject::usage = "
PPLProject[p, vars] projects out vars from p.";

PPLMapVariables::usage = "
PPLMapVariables[p, map] maps the dimension names of p by map.";

PPLAddDimensions::usage = "
PPLAddDimensions[p, dim] adds dim dimensions to p.";

PPLIsEmpty::usage = "
PPLIsEmpty[p] returns if p is empty.";

PPLIsUniverse::usage = "
PPLIsUniverse[p] returns if p is full.";

PPLSpaceDimension::usage = "
PPLSpaceDimension[p] returns the number of dimensions of p.";

PPLIntersect::usage = "
PPLIntersect[p1, p2] sets p1 to the intersection of p1 and p2.  p1 and
p2 must share the same number of dimensions.";

PPLIntersectAndMinimize::usage = "
PPLIntersectAndMinimize[p1, p2] sets p1 to the intersection of p1 and
p2.  The representation for p1 is minimized.  p1 and p2 must share the
same number of dimensions.";

PPLUnion::usage = "
PPLUnion[p1, p2] sets p1 to the convex hull of p1 and p2.  p1 and p2
must share the same number of dimensions.";

PPLUnionAndMinimize::usage = "
PPLUnionAndMinimize[p1, p2] sets p1 to the convex hull of p1 and p2.
The representation for p1 is minimized.  p1 and p2 must share the same
number of dimensions.";

PPLAffineImage::usage = "
PPLAffineImage[p, var, expr] computes the affine image of p under
var = expr.";

PPLContains::usage = "
PPLContains[p1, p2] returns whether p1 contains p2.  p1 and p2 must
share the same number of dimensions.";

PPLIsDisjointFrom::usage = "
PPLIsDisjointFrom[p1, p2] returns whether p1 is disjoint from p2.  p1
and p2 must share the same number of dimensions.";

PPLStandardNarrow::usage = "
PPLStandardNarrow[p1,p2] drops any generator of p1 that is not subsumed by p2.
Note that PPLContains[p1,p2] is expected but not checked currently. "


PPLExtrapolateNarrow::usage = "
PPLExtrapolateNarrow[p1,p2,prec] implements an extrapolation narrowing operator.
Note that PPLContains[p1,p2] is expected but not checked currently. "


Line::usage = " make a line "
Ray::usage = ""
Vertex::usage = ""


Begin["`private`"];

(*** Session Commands ***)

PPLBegin[] := (
    Clear[PolyStore];
    PolyCalcVars = {};
    PPLln = Install["mppl"];
  );

PPLEnd[] := (
    Uninstall[PPLln];
    Clear[PolyStore, PPLln];
  );

(*** Public Interface ***)

(*** Calculator ***)

PolyhedraVariables[
  vars_List
] := PolyCalcVars = Union[vars];

Polyhedron[
  constraints_List /; AreConstraints[constraints]
] :=
With[ {
    cv = ConstraintVariables[constraints]
  },
    If[Intersection[cv, PolyCalcVars] != cv,
        PPLError["Polyhedron: Bad variables: " <> ToString[cv]];
        -1,
        PPLNew[constraints, PolyCalcVars]
      ]
  ];

(* And *)

Intersect[
    p1 : Poly[pid1_Integer],
    p2 : Poly[pid2_Integer]
] /; PolyStore[pid1] == PolyCalcVars && PolyStore[pid2] == PolyCalcVars := (
    PPLIntersectAndMinimize[p1, p2];
    PPLDelete[p2];
    p1
  );

(* Or *)

Hull[
    p1 : Poly[pid1_Integer],
    p2 : Poly[pid2_Integer]
] /; PolyStore[pid1] == PolyCalcVars && PolyStore[pid2] == PolyCalcVars := (
    PPLUnionAndMinimize[p1, p2];
    PPLDelete[p2];
    p1
  );

(* Exists *)

Project[
    vars_List,
    p : Poly[pid_Integer]
] /; (Intersection[PolyStore[pid], PolyCalcVars] == PolyStore[pid]
       && Intersection[vars, PolyCalcVars] == vars) := (
    PPLProject[p, vars];
    p
  );

Project[
    vars_List,
    cond_List /; AreConstraints[cond],
    p : Poly[pid_Integer]
] /; (Intersection[PolyStore[pid], PolyCalcVars] == PolyStore[pid]
       && Intersection[vars, PolyCalcVars] == vars) :=
With[ {PPLBegin[];
Print["Mathlink to PPL established."];
    cts = Intersect[p, Polyhedron[cond]]
  },
    Exists[vars, cts]
  ];

(* PolyhedraApply *)

PolyhedraApply[
    fn_,
    cts1_List /; AreConstraints[cts1],
    cts2_List /; AreConstraints[cts2]
] :=
With[ {
    vars = ConstraintVariables[Join[cts1, cts2]]
  },
    With[ {
        poly1 = PPLNew[cts1, vars],
        poly2 = PPLNew[cts2, vars]
      },
        fn@@{poly1, poly2};
        With[ { cts = PPLConstraints[poly1] },
            PPLDelete[poly1];
            PPLDelete[poly2];
            cts
          ]
      ]
  ];

(*** Direct manipulation ***)

PPLNew[
  constraints0_List /; AreConstraints[constraints0],
  vars0_List:List[]
] :=
With[ {
    constraints = Flatten[Map[NormalizedConstraint[#] &, constraints0]],
    vars = If[vars0 != {}, vars0, ConstraintVariables[constraints0]]
  },
    With[ {
        pid = 
            MLPPLNew[Length[vars], 
                     Join@@Map[Coefficients[#, vars] &, constraints]
              ]
      },
      Switch[pid,
         _Integer,
          If[pid == -1, 
             Print["PPLNew: Failed."]; -1, 
             PolyStore[pid] = vars;
             Poly[pid]
           ] ,
         _, Print[pid];Print["PPLNew: Failed."]; -1
      ]
]  ];

PPLVariables[
  poly : Poly[pid_Integer]
] := PolyStore[pid];

PPLCopy[
  poly : Poly[pid_Integer]
] := 
With[ { 
    npid = MLPPLCopy[pid]
  },
    PolyStore[npid] = PolyStore[pid];
    Poly[npid]
  ];

PPLDelete[
  poly : Poly[pid_Integer]
] := (
    Switch[PolyStore[pid], 
        _List, PolyStore[pid] = .; MLPPLDelete[pid],
        _, PPLError["PPLDelete: Nothing to delete."]
      ]
  );

PPLAddConstraints[
  poly : Poly[pid_Integer],
  constraints0_List /; AreConstraints[constraints0],
  minimize_:False
] :=
With[ {
    constraints = Flatten[Map[NormalizedConstraint[#] &, constraints0]],
    vars = PPLVariables[poly]
  },
    If[vars == {}, PPLError["PPLAddConstraints: Bad PID."]];
    With[ {
        rv = 
            If[minimize,
                MLPPLAddConstraintsAndMinimize[
                    pid, 
                    Join@@Map[Coefficients[#, vars] &, constraints]
                  ],
                MLPPLAddConstraints[
                    pid, 
                    Join@@Map[Coefficients[#, vars] &, constraints]
                  ]
              ]
      },
        If[rv == -1,
            PLLError["PPLConstraints: Failed."]; -1,
            1
          ]
      ]
  ];

PPLAddConstraintsAndMinimize[
  poly : Poly[pid_Integer],
  constraints0_List /; AreConstraints[constraints0]
] :=
With[ {
    constraints = Flatten[Map[NormalizedConstraint[#] &, constraints0]],
    vars = PPLVariables[poly]
  },
    If[vars == {}, PPLError["PPLAddConstraints: Bad PID."]];
    With[ {
        rv = 
            MLPPLAddConstraintsAndMinimize[
                pid, 
                Join@@Map[Coefficients[#, vars] &, constraints]
              ]
      },
        If[rv == -1,
            PLLError["PPLConstraints: Failed."]; -1,
            1
          ]
      ]
  ];

PPLConstraints[
  poly : Poly[pid_Integer]
] :=
With[ {
    vars = PolyStore[pid],
    dim = MLPPLSpaceDimension[pid],
    coeffs = MLPPLConstraints[pid]
  },
    If[dim == -1,
        PPLError["PPLConstraints: Bad PID: " <> ToString[pid]],
        Map[InflatedConstraint[
                With[ { offset = ((dim + 2) * #) + 1 },
                    Take[coeffs, {offset, offset + dim + 1}]
                  ],
                vars
              ] &,
            Range[0, Length[coeffs] / (dim + 2) - 1]
          ]
      ]
  ];

PPLVertices[
  poly : Poly[pid_Integer]
] :=
With[ {
    vars = PolyStore[pid],
    dim = MLPPLSpaceDimension[pid],
    coeffs = MLPPLPoints[pid]
  },
    If[dim == -1,
        PPLError["PPLConstraints: Bad PID: " <> ToString[pid]],
        Map[
            With[ { offset = ((dim + 1) * #) + 1 },
                Take[coeffs, {offset, offset + dim - 1}]
                  / coeffs[[offset + dim]]
              ] &,
            Range[0, Length[coeffs] / (dim + 1) - 1]
          ]
      ]
  ];

PPLGenerators[ 
  poly: Poly[pid_Integer] 
]:= 
  With[ {
    dim = MLPPLSpaceDimension[pid],
    res = MLPPLGenerators[pid]
  },
    If[dim == -1,
        PPLError["PPLConstraints: Bad PID: " <> ToString[pid]],
        res ]
];



PPLProject[
  poly : Poly[pid_Integer],
  pvars_List
] := 
With[ {
    vars = PolyStore[pid]
  },
    With[ {
        dims = Flatten[Map[Position[vars, #] &, pvars]] - 1
      },
        If[MLPPLProject[pid, dims] == -1,
            PPLError["PPLProject: Failed."]; -1,
            PolyStore[pid] = Select[vars, !MemberQ[pvars, #] &];
            1
          ]
      ]
  ];

PPLMapVariables[
  poly : Poly[pid_Integer],
  map : List[first_Rule, rest___]
] := PolyStore[pid] = PolyStore[pid] /. map;

PPLAddDimensions[
  poly : Poly[pid_Integer],
  nvars_List
] :=
With[ {
    vars = Join[PolyStore[pid], nvars]
  },
    If[MLPPLAddDimensionsAndEmbed[pid, Length[nvars]] == -1,
        PPLError["PPLAddDimensions: Failed."]; -1,
        PolyStore[pid] = vars;
        1
      ]
  ];

PPLIsEmpty[
  poly : Poly[pid_Integer]
] := ToBoolean[MLPPLIsEmpty[pid]];

PPLIsUniverse[
  poly : Poly[pid_Integer]
] := ToBoolean[MLPPLIsUniverse[pid]];

PPLSpaceDimension[
  poly : Poly[pid_Integer]
] := MLPPLSpaceDimension[pid];

PPLIntersect[
  poly1 : Poly[pid1_Integer],
  poly2 : Poly[pid2_Integer]
] := 
If[!(PolyStore[pid1] === PolyStore[pid2]),
    PPLError["PPLIntersect: Domains unmatched: " 
     <> ToString[pid1] <> ", " 
     <> ToString[pid2]]; 
    -1,
    MLPPLIntersect[pid1, pid2]
  ];

PPLIntersectAndMinimize[
  poly1 : Poly[pid1_Integer],
  poly2 : Poly[pid2_Integer]
] := 
If[!(PolyStore[pid1] === PolyStore[pid2]),
    PPLError["PPLIntersectAndMinimize: Domains unmatched: "
     <> ToString[pid1] <> ", " 
     <> ToString[pid2]]; 
    -1,
    MLPPLIntersectAndMinimize[pid1, pid2]
  ];

PPLUnion[
  poly1 : Poly[pid1_Integer],
  poly2 : Poly[pid2_Integer]
] := 
If[!(PolyStore[pid1] === PolyStore[pid2]),
    PPLError["PPLUnion: Domains unmatched: "
     <> ToString[pid1] <> ", " 
     <> ToString[pid2]]; 
    -1,
    MLPPLUnion[pid1, pid2]
  ];

PPLUnionAndMinimize[
  poly1 : Poly[pid1_Integer],
  poly2 : Poly[pid2_Integer]
] := 
If[!(PolyStore[pid1] === PolyStore[pid2]),
    PPLError["PPLUnionAndMinimize: Domains unmatched: "
     <> ToString[pid1] <> ", " 
     <> ToString[pid2]]; 
    -1,
    MLPPLUnionAndMinimize[pid1, pid2]
  ];

PPLAffineImage[
  poly : Poly[pid_Integer],
  var_Symbol,
  expr0_ /; MemberQ[{Plus, Times, Symbol}, Head[expr]]
] := PPLAffineImage[poly, {var}, {expr0}];

PPLAffineImage[
  poly : Poly[pid_Integer],
  vars_List /; And@@Map[Head[#] === Symbol &, vars],
  exprs0_List /; And@@Map[MemberQ[{Plus, Times, Symbol}, Head[#]] &, exprs0]
] :=
With[ {
    pvars = PolyStore[pid]
  },
    If[pvars == {}, PPLError["PPLAffineImage: Bad PID."]];

    With[ {
        dims = Flatten[Map[Position[pvars, #] &, vars]] - 1,
        lcms = Map[LCM@@FindLCM[#] &, exprs0],
        exprs = Map[Coefficients[#, pvars] &, exprs0]
      },
        With[ {
            exprl = Join@@Map[Append[exprs[[#]], lcms[[#]]] &, 
                              Range[Length[exprs0]]]
          },
            MLPPLAffineImage[pid, dims, exprl]
          ]
      ]
  ];

PPLContains[
  poly1 : Poly[pid1_Integer],
  poly2 : Poly[pid2_Integer]
] := 
If[!(PolyStore[pid1] === PolyStore[pid2]),
    PPLError["PPLContains: Domains unmatched: "
     <> ToString[pid1] <> ", " 
     <> ToString[pid2]]; 
    -1,
    ToBoolean[MLPPLContains[pid1, pid2]]
  ];

PPLIsDisjointFrom[
  poly1 : Poly[pid1_Integer],
  poly2 : Poly[pid2_Integer]
] := 
If[!(PolyStore[pid1] === PolyStore[pid2]),
    PPLError["PPLIsDisjointFrom: Domains unmatched: "
     <> ToString[pid1] <> ", " 
     <> ToString[pid2]]; 
    -1,
    ToBoolean[MLPPLIsDisjointFrom[pid1, pid2]]
  ];


PPLExtrapolateNarrow[
  poly1: Poly[pid1_Integer],
  poly2: Poly[pid2_Integer],
  prec_Integer
] := 
If [!(PolyStore[pid1] === PolyStore[pid2]),
  PPLError["PPLExtrapolateNarrow: Domains unmatched: "
     <> ToString[pid1] <> ", " 
     <> ToString[pid2]]; 
    -1, 
    With[ { vars= PolyStore[pid1], retp = MLPPLExtrapolateNarrow[pid1,pid2,prec]},
       If[retp == -1, 
              PPLError["PPLExtrapolateNarrow: Failed."]; -1, 
              PolyStore[retp] = vars;
              Poly[retp]
        ]
]
];


PPLStandardNarrow[
  poly1: Poly[pid1_Integer],
  poly2: Poly[pid2_Integer]
] := 
If [!(PolyStore[pid1] === PolyStore[pid2]),
  PPLError["PPLStandardNarrow: Domains unmatched: "
     <> ToString[pid1] <> ", " 
     <> ToString[pid2]]; 
    -1, 
    With[ { vars= PolyStore[pid1], retp = MLPPLStandardNarrow[pid1,pid2]},
       If[retp == -1, 
              PPLError["PPLStandardNarrow: Failed."]; -1, 
              PolyStore[retp] = vars;
              Poly[retp]
        ]
]
];


(*** Private ***)

PPLError[
  msg_String
] := Print[msg];

AreConstraints[
  constraints_List
] := 
And@@Map[
    IsRelation[Head[#]] || # === True || # === False &,
    constraints
  ];

IsRelation[
  rel_
] := MemberQ[{LessEqual, GreaterEqual, Equal}, rel];

ConstraintVariables[
  constraints_List
] :=
Union@@Map[Union[Variables[#[[1]]], Variables[#[[2]]]] &, 
           constraints
  ];

NormalizedConstraint[
  constraint0_ /; IsRelation[Head[constraint0]]
] :=
With[ {
    expr = ToIntegers[constraint0[[1]] - constraint0[[2]]]
  },
    Switch[constraint0,
        _Equal, { expr >= 0, -1 * expr >= 0 },
        _LessEqual, -1 * expr >= 0,
        _GreaterEqual, expr >= 0
      ]
  ];

NormalizedConstraint[
  constraint0_ /; (constraint0 === False || constraint0 === True)
] := constraint0;

ToIntegers[
  expr_
] :=
With[ { lcm = LCM@@FindLCM[expr] },
    Expand[lcm * expr]
  ];

FindLCM[
  expr_Plus
] := Join@@Map[FindLCM[#] &, List@@expr];

FindLCM[
  expr : Times[left_, right_]
] := Join[FindLCM[left], FindLCM[right]];

FindLCM[
  expr : Rational[num_, denom_]
] := {denom};

FindLCM[
  expr_Symbol
] := {1};

FindLCM[
  expr_Integer
] := {1};

Coefficients[
  expr_GreaterEqual,
  vars_List
] := Coefficients[expr[[1]], vars];

Coefficients[
  expr_ /; (expr === False),
  vars_List
] := Append[Array[0 &, Length[vars]], -1];

Coefficients[
  expr_ /; (expr === True),
  vars_List
] := Append[Array[0 &, Length[vars]], 1];

Coefficients[
  expr_ /; MemberQ[{Plus, Times, Symbol}, Head[expr]],
  vars_List
] := 
With[ { tozeros = Map[# -> 0 &, vars] },
    With[ { constant = expr /. tozeros },
       Append[
            Map[(expr - constant) /. {# -> 1} /. tozeros &, vars],
            constant
          ]
      ]
  ];

AdjPrec[c_List,prec_Integer] := 
  Module[ {min,problem,minfound},
   problem = False;
   min=1;
   minfound = False;
   For[i=1,i<=Length[c],i++,
      If[Abs[c[[i]]] >= prec, problem=True];
      If[ c[[i]] != 0 && (!minfound || Abs[c[[i]]] > min), 
          min = Abs[c[[i]]]]; minfound = True;
      ];
      If [problem,
        Print["Rounding"];
        Map[ Round[#/1000]&, c],
        c]
];


InflatedConstraint[
  coeffs_List,
  vars_List
] /; Length[coeffs] - 2 == Length[vars] :=
With[ {
    expr = Inner[Times, Drop[coeffs, -2], vars, Plus] 
               /. {0. -> 0, 1. -> 1, -1. -> -1},
    inhom = coeffs[[-2]],
    rel = Which[coeffs[[-1]] == 0, GreaterEqual, coeffs[[-1]] == 1, Equal]
  },
    rel[expr, -inhom]
  ];

ToBoolean[
  int_Integer
] := If[int > 0, True, False];

End[];
EndPackage[];

PPLBegin[];
Print["MathLink to PPL is loaded."];
