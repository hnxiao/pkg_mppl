#include "ppl.hh"
#include "mathlink.h"
#include "narrowing.h"

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;


extern int MLPPLNew(int dim, int * p, long sz);
extern int MLPPLCopy(int p);
extern int MLPPLDelete(int p);
extern int MLPPLAddConstraints(int p, int * cts, long sz);
extern int MLPPLAddConstraintsAndMinimize(int p, int * cts, long sz);
extern void MLPPLConstraints(int p);
extern void MLPPLGenerators(int p);
extern void MLPPLPoints(int p);
extern int MLPPLProject(int p, int * rmdim, long szrd);
extern int MLPPLAddDimensionsAndEmbed(int p, int n);
extern int MLPPLIsEmpty(int p);
extern int MLPPLIsUniverse(int p);
extern int MLPPLSpaceDimension(int p);
extern int MLPPLIntersect(int p1, int p2);
extern int MLPPLIntersectAndMinimize(int p1, int p2);
extern int MLPPLUnion(int p1, int p2);
extern int MLPPLUnionAndMinimize(int p1, int p2);
extern int MLPPLAffineImage(int p, int var, int * expr, long sz);
extern int MLPPLAffineImage(int p, int * vars, long vsz, 
			    int * exprs, long esz);
extern int MLPPLContains(int p1, int p2);
extern int MLPPLIsDisjointFrom(int p1, int p2);
extern int MLPPLStandardNarrow(int p1, int p2);// compute the standard narrowing.
extern int MLPPLExtrapolateNarrow(int p1, int p2, long precision); // compute an extrapolation operation

static int new_poly(int dim); // universal polyhedron by default
static int new_empty_poly(int dim);
static int del_poly(int pi);
static int add_constraints(int pi, int dim, int * p, long sz, bool min);
static int affine_image(int p, int * vars, long vsz, int * exprs, long esz);
static NNC_Polyhedron * poly_store(int pi);
static int put_poly(int pi);
static int put_points(int p);
static int put_generators(int p);
static int standard_narrow( int p1, int p2); // drop generators of p1 that are not in p2.




/*
 * MathLink visible functions.
 */

int MLPPLNew(int dim, int * p, long sz) {
  int pi = new_poly(dim);
  if (pi == -1) { cerr<<"Error calling new_poly"; return -1;}
  if (add_constraints(pi, dim, p, sz, true) == -1) {
    del_poly(pi);
    return -1;
  }
  return pi;
}


int MLPPLStandardNarrow( int p1 , int p2){
   
   if (! poly_store(p1)) return -1;
   if (!poly_store(p2)) return -1;
   
   return standard_narrow(p1,p2);
      
}

int MLPPLExtrapolateNarrow( int p1 , int p2, int prec){
   NNC_Polyhedron * poly1 = poly_store(p1);
   NNC_Polyhedron * poly2 = poly_store(p2);
   if (!poly1) return -1;
   if (!poly2) return -1;

   int ret_pid = new_poly(poly1->space_dimension());

   NNC_Polyhedron * res = poly_store(ret_pid);

   if (extrapolation_narrowing(poly1, poly2, res, (long) prec) != -1)     
      return ret_pid;

   
   return -1;
}



int MLPPLCopy(int p) {
  if (!poly_store(p)) return -1;

  int pi = new_poly(poly_store(p)->space_dimension());
  if (pi == -1) return -1;
  *poly_store(pi) = *poly_store(p);
  return pi;
}

int MLPPLDelete(int p) {
  return del_poly(p);
}

int MLPPLAddConstraints(int p, int * cts, long sz) {
  if (!poly_store(p)) return -1;

  if (add_constraints(p, MLPPLSpaceDimension(p), cts, sz, false) == -1)
    return -1;

  return 1;
}

int MLPPLAddConstraintsAndMinimize(int p, int * cts, long sz) {
  if (!poly_store(p)) return -1;

  if (add_constraints(p, MLPPLSpaceDimension(p), cts, sz, true) == -1)
    return -1;

  return 1;
}

void MLPPLConstraints(int p) {
  if (put_poly(p) == -1) {
    int rv[1];
    rv[0] = -1;
    MLPutIntegerList(stdlink, rv, 1);
  }
}

void MLPPLGenerators(int p){
   if (put_generators(p) == -1) {
     int rv[1];
     rv[0] = -1;
     MLPutIntegerList(stdlink, rv, 1);
   }

}

void MLPPLPoints(int p) {
  if (put_points(p) == -1) {
    int rv[1];
    rv[0] = -1;
    MLPutIntegerList(stdlink, rv, 1);
  }
}

int MLPPLProject(int p, int * rmdim, long szrd) {
  if (!poly_store(p)) return -1;

  Variables_Set rem_set;
  for(int i = 0; i < szrd; ++i) {
    rem_set.insert(Variable(rmdim[i]));
  }
  poly_store(p)->remove_space_dimensions(rem_set);
  return 1;
}

int MLPPLAddDimensionsAndEmbed(int p, int n) {
  if (!poly_store(p)) return -1;
  poly_store(p)->add_space_dimensions_and_embed(n);
  return 1;
}

int MLPPLIsEmpty(int p) {
  if (!poly_store(p)) return -1;
  return poly_store(p)->is_empty() ? 1 : 0;
}

int MLPPLIsUniverse(int p) {
  if (!poly_store(p)) return -1;
  return poly_store(p)->is_universe() ? 1 : 0;
}

int MLPPLSpaceDimension(int p) {
  if (!poly_store(p)) return -1;
  return poly_store(p)->space_dimension();
}

int MLPPLIntersect(int p1, int p2) {
  if (!poly_store(p1) || !poly_store(p2)) return -1;
  poly_store(p1)->intersection_assign(*poly_store(p2));
  return 1;
}

int MLPPLIntersectAndMinimize(int p1, int p2) {
  if (!poly_store(p1) || !poly_store(p2)) return -1;
  poly_store(p1)->intersection_assign_and_minimize(*poly_store(p2));
  return 1;
}

int MLPPLUnion(int p1, int p2) {
  if (!poly_store(p1) || !poly_store(p2)) return -1;
  poly_store(p1)->poly_hull_assign(*poly_store(p2));
  return 1;
}

int MLPPLUnionAndMinimize(int p1, int p2) {
  if (!poly_store(p1) || !poly_store(p2)) return -1;
  poly_store(p1)->poly_hull_assign_and_minimize(*poly_store(p2));
  return 1;
}

int MLPPLAffineImage(int p, int var, int * expr, long sz) {
  int vara[1];
  vara[0] = var;
  return MLPPLAffineImage(p, vara, 1, expr, sz);
}

int MLPPLAffineImage(int p, int * vars, long vsz, int * exprs, long esz) {
  return affine_image(p, vars, vsz, exprs, esz);
}

int MLPPLContains(int p1, int p2) {
  if (!poly_store(p1) || !poly_store(p2)) return -1;
  return poly_store(p1)->contains(*poly_store(p2)) ? 1 : 0;
}

int MLPPLIsDisjointFrom(int p1, int p2) {
  if (!poly_store(p1) || !poly_store(p2)) return -1;
  return poly_store(p1)->is_disjoint_from(*poly_store(p2)) ? 1 : 0;
}

/*
 * Utilities.
 */

#define PSTORE_MAX 262144
static NNC_Polyhedron * pstore[PSTORE_MAX];
int newid;

#define INC(id) id = (id + 1) % PSTORE_MAX

void init_store() {
  for(int i = 0; i < PSTORE_MAX; ++i)
    pstore[i] = NULL;
  newid = -1;
}

int new_empty_poly(int dim){

   INC(newid);
   while (pstore[newid])
      INC(newid);
   
   pstore[newid] = new NNC_Polyhedron(dim,Polyhedron::EMPTY);
   return newid;
}

int new_poly(int dim) {
  // Possibility of infinite loop.
  INC(newid);
  while(pstore[newid]) 
    INC(newid);

  pstore[newid] = new NNC_Polyhedron(dim);
  return newid;
}

int del_poly(int p) {
  if (!pstore[p]) return -1;
  delete pstore[p];
  pstore[p] = NULL;
  return 1;
}

NNC_Polyhedron * poly_store(int p) {
  if (p < 0 || p >= PSTORE_MAX) return NULL;
  return pstore[p];
}

int add_constraints(int poly, int dim, int * p, long sz, bool min) {
  if (!poly_store(poly)) return -1;

  Constraint_System cs;
  int npoly = sz / (dim + 1);
  for(int pi = 0; pi < npoly; ++pi) {
    int * pcoeffs = p + pi * (dim + 1);

    // Initialize to constant.
    Linear_Expression expr = Linear_Expression(pcoeffs[dim]);
    // Add coeff * var terms.
    for(int vi = 0; vi < dim; ++vi)
      expr += pcoeffs[vi] * Variable(vi);

    cs.insert(expr >= 0);
  }
  if (min) poly_store(poly)->add_constraints_and_minimize(cs);
  else poly_store(poly)->add_constraints(cs);

  return 1;
}

int affine_image(int p, int * vars, long vsz, int * exprs, long esz) {
  if (!poly_store(p)) return -1;

  /*
   * Format:
   * (<coeffs>, <constant>, <divisor>)+
   */

  int dim = MLPPLSpaceDimension(p);
  int nexpr = esz / (dim + 2);

  if (vsz != nexpr) return -1;

  for(int ai = 0; ai < nexpr; ++ai) {
    int * expr = exprs + (dim + 2) * ai;

    Linear_Expression lexpr = Linear_Expression(expr[dim]);
    for(int vi = 0; vi < dim; ++vi)
      lexpr += expr[vi] * Variable(vi);

    poly_store(p)->affine_image(Variable(vars[ai]), lexpr, expr[dim + 1]);
  }

  return 1;
}

bool contains_generator (NNC_Polyhedron * p, Generator const & what){
   return  (p -> relation_with (what) == Poly_Gen_Relation::subsumes());
}

int standard_narrow( int p1, int p2){

   NNC_Polyhedron * poly1  = poly_store(p1);
   NNC_Polyhedron * poly2  = poly_store(p2);

   assert(poly1-> space_dimension() == poly2 -> space_dimension());

   int n = (int) poly1->space_dimension();
   
   int ret = new_poly(n);
   int i;
  
   NNC_Polyhedron * retp = poly_store(ret);

   for (i=0;i < n; ++i){
      retp->add_constraint(Variable(i)==0);
   }
   
   Generator_System const & gs = poly1 -> minimized_generators();
   Generator_System::const_iterator vi;

   for (vi = gs.begin(); vi != gs.end(); ++vi){
      if (contains_generator(poly2,(*vi))) {
         retp->add_generator((*vi));
      }
   }

   return ret;
}

typedef int coeff_type;
#define get_ct() get_si()
#define MLPutCTList MLPutIntegerList
#define MLPutCT MLPutInteger


int put_poly(int p) {
  NNC_Polyhedron * poly = poly_store(p);
  if (!poly) return -1;

  /*
   * Format:
   * (<coeffs>, <inhom. term>, <0 if >=, 1 if ==>)+
   */

  const Constraint_System cs = poly->minimized_constraints();

  int ncs = 0;
  for(Constraint_System::const_iterator ci = cs.begin();
      ci != cs.end(); ++ci, ++ncs);

  unsigned int dim = poly->space_dimension();
  coeff_type * coeffs = new coeff_type[ncs * (dim + 2)];
  coeff_type * coeff = coeffs;

  for(Constraint_System::const_iterator ci = cs.begin();
      ci != cs.end(); ++ci) {
    for(unsigned int co = 0; co < ci->space_dimension(); ++co)
      *(coeff++) = (coeff_type) ci->coefficient(Variable(co)).get_ct();
    for(unsigned int co = ci->space_dimension(); co < dim; ++co)
      *(coeff++) = (coeff_type) 0;
    *(coeff++) = (coeff_type) ci->inhomogeneous_term().get_ct();

    *(coeff++) = ci->is_equality() ? 1 : 0;
  }
  MLPutCTList(stdlink, coeffs, ncs * (dim + 2));

  delete[] coeffs;

  return 1;
}

void make_ct_array( coeff_type * coeff, Generator const & g, unsigned int dim){
  unsigned int i;

  for(i =0; i < g.space_dimension(); ++i){
    *(coeff++)= (coeff_type) g.coefficient(Variable(i)).get_ct();
  }
  for(; i < dim; ++i){
    *(coeff++)= (coeff_type) 0;
  }
  
  /* done */
}

int put_generators(int p) {

  NNC_Polyhedron * poly = poly_store(p);
  if (!poly) return -1;
  
  /*
   * Format: 
   * (Vert|Ray|Line)({Coeffs},divisor)+
   *
   */
   
  const Generator_System gs = poly->minimized_generators();
  Generator_System::const_iterator vgs;
  
  int ngs =0;

  /* Count the number of generators */

  for (vgs=gs.begin(); vgs!=gs.end(); ++vgs,++ngs);

  /* make a list of things to put */

  MLPutFunction(stdlink, "List",ngs);
  unsigned int dim = poly->space_dimension();
  coeff_type * coeffs = new coeff_type[dim];
  
  
  for (vgs=gs.begin(); vgs!=gs.end(); ++vgs){
    if (vgs -> is_point()){
      MLPutFunction(stdlink,"Vertex",2);
      make_ct_array(coeffs,(*vgs),dim);
      MLPutCTList(stdlink,coeffs,dim);
      MLPutCT ( stdlink, (coeff_type) vgs->divisor().get_ct());
      
    } else if (vgs -> is_ray()){

      MLPutFunction(stdlink,"Ray",1);
      make_ct_array(coeffs,(*vgs),dim);
      MLPutCTList(stdlink,coeffs,dim);

    } else if (vgs -> is_line()){

      MLPutFunction(stdlink,"Line",1);
      make_ct_array(coeffs,(*vgs),dim);
      MLPutCTList(stdlink,coeffs,dim);
    }

  }
  
  
  delete [] coeffs;
  return 1;
}

int put_points(int p) {
  NNC_Polyhedron * poly = poly_store(p);
  if (!poly) return -1;

  /*
   * Format:
   * (<coeffs>, <divisor>)+
   */

  const Generator_System gs = poly->minimized_generators();

  int ngs = 0;
  for(Generator_System::const_iterator gi = gs.begin();
      gi != gs.end(); ++gi)
    if (gi->is_point()) ++ngs;

  unsigned int dim = poly->space_dimension();
  coeff_type * coeffs = new coeff_type[ngs * (dim + 1)];
  coeff_type * coeff = coeffs;

  for(Generator_System::const_iterator gi = gs.begin();
      gi != gs.end(); ++gi)
    if (gi->is_point()) {
      for(unsigned int co = 0; co < gi->space_dimension(); ++co)
	*(coeff++) = (coeff_type) gi->coefficient(Variable(co)).get_ct();
      for(unsigned int co = gi->space_dimension(); co < dim; ++co)
	*(coeff++) = 0;
      *(coeff++) = (coeff_type) gi->divisor().get_ct();
    }
  MLPutCTList(stdlink, coeffs, ngs * (dim + 1));

  delete[] coeffs;

  return 1;
}

/*
 * Passes control to MathLink.
 */

int main(int argc, char * argv[]) {
  init_store();
  return MLMain(argc, argv);
}












