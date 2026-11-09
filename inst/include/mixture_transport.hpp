#ifndef BAYESIAN_LOT_MIXTURE_TRANSPORT_HPP
#define BAYESIAN_LOT_MIXTURE_TRANSPORT_HPP
#include <cmath>
#include <limits>
#include <stdexcept>
#include <algorithm>

// Exact distributional transport, solved to double precision. No approximation
// to the mixture distribution and no discrete mixture indicators are sampled.
namespace lot_transport {
struct Result { double value, dz, dsigma, residual; int iterations; };
inline double logadd(double a, double b) {
  double m=std::max(a,b);
  if (!std::isfinite(m)) return m;
  return m+std::log1p(std::exp(std::min(a,b)-m));
}
inline double logphi(double a) { return -.5*a*a-.91893853320467274178; }
inline double logsurv(double a) {
  // Only a>=0. erfc avoids subtraction from one. The alternating Mills
  // expansion past26 has an omitted term far below double precision.
  if (a<26) return std::log(.5*std::erfc(a*.70710678118654752440));
  if (!std::isfinite(a)) return -std::numeric_limits<double>::infinity();
  double inv2=1/(a*a), term=1, series=1;
  for (int k=1;k<=16;++k) { term*=-(2*k-1)*inv2; series+=term; }
  return logphi(a)-std::log(a)+std::log(series);
}
inline Result evaluate(double z,double sigma,double q,double extra) {
  if (!std::isfinite(z)||!std::isfinite(sigma)||sigma<=0||q<0||q>1||extra<0)
    throw std::domain_error("Invalid normal-mixture transport arguments");
  double broad=std::hypot(sigma,extra);
  if(q==0||extra==0) return {sigma*z,sigma,z,0,0};
  if(q==1) return {broad*z,broad,z*sigma/broad,0,0};
  if(z==0) return {0,1/((1-q)/sigma+q/broad),0,0,0};
  const double az=std::fabs(z), lq=std::log(q), lr=std::log1p(-q);
  const bool central=az<.1;
  const double target=central ? std::erf(az*.70710678118654752440) : logsurv(az);
  double lo=sigma*az, hi=broad*az;
  double x=std::min(hi,std::max(lo,az/((1-q)/sigma+q/broad)));
  double residual=0, logf=0, ar=0, ab=0;
  int it=0;
  for(;it<120;++it) {
    ar=lr+logphi(x/sigma)-std::log(sigma);
    ab=lq+logphi(x/broad)-std::log(broad);
    logf=logadd(ar,ab);
    double candidate;
    if(central) {
      double mass=(1-q)*std::erf(x/sigma*.70710678118654752440)+q*std::erf(x/broad*.70710678118654752440);
      residual=mass-target;
      if(std::fabs(residual)<=2e-14*target) break;
      if(residual>0) hi=x; else lo=x;
      candidate=x-residual/(2*std::exp(logf));
    } else {
      double ls=logadd(lr+logsurv(x/sigma),lq+logsurv(x/broad));
      residual=ls-target;
      if(std::fabs(residual)<=2e-13) break;
      if(residual>0) lo=x; else hi=x;
      candidate=x+residual*std::exp(ls-logf);
    }
    if(!std::isfinite(candidate)||candidate<=lo||candidate>=hi) candidate=lo+(hi-lo)/2;
    if(candidate==x) break;
    x=candidate;
  }
  if(it==120) throw std::domain_error("Normal-mixture transport root did not converge");
  ar=lr+logphi(x/sigma)-std::log(sigma);
  ab=lq+logphi(x/broad)-std::log(broad);
  logf=logadd(ar,ab);
  double delta=std::copysign(x,z);
  double dz=std::exp(logphi(az)-logf);
  double ds=delta*(std::exp(ar-logf)/sigma+std::exp(ab-logf)*sigma/broad/broad);
  return {delta,dz,ds,residual,it};
}
}

#ifndef LOT_TRANSPORT_STANDALONE
// Stan's external function interface. q and extra are data, so gradients
// are required only for z and sigma. Derivatives use the implicit-function
// identity f(delta)*d(delta)/dz=phi(z), not derivatives of root-finder branches.
template <typename TZ, typename TS>
inline stan::return_type_t<TZ,TS> normal_mixture_transport(
    const TZ& z,const TS& sigma,double q,double extra,std::ostream*) {
  const auto r=lot_transport::evaluate(stan::math::value_of(z),stan::math::value_of(sigma),q,extra);
  auto ops=stan::math::make_partials_propagator(z,sigma);
  if(!stan::is_constant_all<TZ>::value) stan::math::partials<0>(ops)=r.dz;
  if(!stan::is_constant_all<TS>::value) stan::math::partials<1>(ops)=r.dsigma;
  return ops.build(r.value);
}
#endif
#endif
