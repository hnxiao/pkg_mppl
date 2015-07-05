:Evaluate: BeginPackage["MPPL`"]
:Evaluate: Begin["`private`"]

:Begin:
:Function: MLPPLNew
:Pattern: MLPPLNew[dim_Integer, constraints_List]
:Arguments: {dim, constraints}
:ArgumentTypes: {Integer, IntegerList}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLCopy
:Pattern: MLPPLCopy[p_Integer]
:Arguments: {p}
:ArgumentTypes: {Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLDelete
:Pattern: MLPPLDelete[p_Integer]
:Arguments: {p}
:ArgumentTypes: {Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLAddConstraints
:Pattern: MLPPLAddConstraints[p_Integer, constraints_List]
:Arguments: {p, constraints}
:ArgumentTypes: {Integer, IntegerList}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLAddConstraintsAndMinimize
:Pattern: MLPPLAddConstraintsAndMinimize[p_Integer, constraints_List]
:Arguments: {p, constraints}
:ArgumentTypes: {Integer, IntegerList}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLConstraints
:Pattern: MLPPLConstraints[p_Integer]
:Arguments: {p}
:ArgumentTypes: {Integer}
:ReturnType: Manual
:End:


:Begin:
:Function: MLPPLGenerators
:Pattern: MLPPLGenerators[p_Integer]
:Arguments: {p}
:ArgumentTypes: {Integer}
:ReturnType: Manual
:End:

:Begin:
:Function: MLPPLPoints
:Pattern: MLPPLPoints[p_Integer]
:Arguments: {p}
:ArgumentTypes: {Integer}
:ReturnType: Manual
:End:

:Begin:
:Function: MLPPLProject
:Pattern: MLPPLProject[p_Integer, vars_List]
:Arguments: {p, vars}
:ArgumentTypes: {Integer, IntegerList}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLAddDimensionsAndEmbed
:Pattern: MLPPLAddDimensionsAndEmbed[p_Integer, n_Integer]
:Arguments: {p, n}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLIsEmpty
:Pattern: MLPPLIsEmpty[p_Integer]
:Arguments: {p}
:ArgumentTypes: {Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLIsUniverse
:Pattern: MLPPLIsUniverse[p_Integer]
:Arguments: {p}
:ArgumentTypes: {Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLSpaceDimension
:Pattern: MLPPLSpaceDimension[p_Integer]
:Arguments: {p}
:ArgumentTypes: {Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLIntersect
:Pattern: MLPPLIntersect[p1_Integer, p2_Integer]
:Arguments: {p1, p2}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLIntersectAndMinimize
:Pattern: MLPPLIntersectAndMinimize[p1_Integer, p2_Integer]
:Arguments: {p1, p2}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLUnion
:Pattern: MLPPLUnion[p1_Integer, p2_Integer]
:Arguments: {p1, p2}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLUnionAndMinimize
:Pattern: MLPPLUnionAndMinimize[p1_Integer, p2_Integer]
:Arguments: {p1, p2}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLAffineImage
:Pattern: MLPPLAffineImage[p_Integer, var_Integer, expr_List]
:Arguments: {p, var, expr}
:ArgumentTypes: {Integer, Integer, IntegerList}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLAffineImage
:Pattern: MLPPLAffineImage[p_Integer, vars_List, expr_List]
:Arguments: {p, vars, expr}
:ArgumentTypes: {Integer, IntegerList, IntegerList}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLContains
:Pattern: MLPPLContains[p1_Integer, p2_Integer]
:Arguments: {p1, p2}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:


:Begin:
:Function: MLPPLStandardNarrow
:Pattern: MLPPLStandardNarrow[p1_Integer, p2_Integer]
:Arguments: {p1, p2}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:


:Begin:
:Function: MLPPLExtrapolateNarrow
:Pattern: MLPPLExtrapolateNarrow[p1_Integer, p2_Integer, prec_Integer]
:Arguments: {p1, p2, prec}
:ArgumentTypes: {Integer, Integer, Integer}
:ReturnType: Integer
:End:

:Begin:
:Function: MLPPLIsDisjointFrom
:Pattern: MLPPLIsDisjointFrom[p1_Integer, p2_Integer]
:Arguments: {p1, p2}
:ArgumentTypes: {Integer, Integer}
:ReturnType: Integer
:End:

:Evaluate: End[]
:Evaluate: EndPackage[]
