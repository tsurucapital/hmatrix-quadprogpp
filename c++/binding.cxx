# include <QuadProg++/QuadProg++.hh>
# include <exception>
# include <string.h>

/* These namespaces may or may not be defined, depending on the version of
 * QuadProg++. Here we make sure the are all defined, so that we can be `using`
 * them later.
 */

namespace QuadProgPP{}
namespace quadprogpp{}

extern "C" double hs_solve_quadprog(
  int n_vars, int n_ce, int n_ci,
  const double *G_,
  const double *g0_,
  const double *CE_,
  const double *ce0_,
  const double *CI_,
  const double *ci0_,
  double *x_,
  const char **p_errorstr) try
{
  /* Depending on the version, the names may be in the global namespace, the
   * QuadProgPP namespace or the quadprogpp namespace.
   */
  using namespace QuadProgPP;
  using namespace quadprogpp;
  Matrix<double> G(G_, n_vars, n_vars);
  Vector<double> g0(g0_, n_vars);
  Matrix<double> CE(CE_, n_vars, n_ce);
  Vector<double> ce0(ce0_, n_ce);
  Matrix<double> CI(CI_, n_vars, n_ci);
  Vector<double> ci0(ci0_, n_ci);
  Vector<double> x;
  double r = solve_quadprog(G, g0, CE, ce0, CI, ci0, x);
  for(int i = 0; i < n_vars; i++)
    x_[i] = x[i];
  *p_errorstr = 0;
  return r;
} catch(const std::exception &e) {
  *p_errorstr = strdup(e.what());
  return 0;
} catch(...) {
  *p_errorstr = strdup("unknown C++ error");
  return 0;
}
