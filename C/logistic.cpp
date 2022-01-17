// logistic fit with false positive/negative, dbetabinom/sigma param
#include <TMB.hpp>
#include "logistic_fit.h"

template<class Type>
Type objective_function<Type>::operator() ()
{

	DATA_FACTOR(prov);
	DATA_IVECTOR(time);       // time
	DATA_INTEGER(nprov);		  // num provinces: should compute from data?
	DATA_VECTOR(omicron);	  // number of recorded SGTF dropouts
	// (make this float rather than int so that we can use TMB's rbinom() in simulation step)
	DATA_IVECTOR(tot);  // number of confirmed cases
	DATA_VECTOR(reinf); // reinfections, possibly NA (in which case param will be mapped to 0)
	DATA_INTEGER(debug);		  // debugging flag
	DATA_INTEGER(perfect_tests);

	DATA_VECTOR(prior_logsd_logdeltar);	  // prior mean, sd
	DATA_VECTOR(prior_logsd_reinf);	  // prior mean, sd
  
	PARAMETER_VECTOR(loc);	  // midpoint of takeover curve (fixed effect)
	PARAMETER(log_deltar);	      // takeover rate
	PARAMETER(beta_reinf);		// effect of reinfection on prob of omicron
	// FIXME: add random effects for reinf; correlated with deltar variation?
	PARAMETER_VECTOR(b_logdeltar);		  // random effects vector: {deltar} x nprov
	PARAMETER_VECTOR(b_reinf);		  // random effects vector: {deltar} x nprov
	PARAMETER(lodrop);		  // log-odds of false negative for SGTF (universal)
	PARAMETER(logain);		  // log-odds of false positive for SGTF (universal)
	PARAMETER(log_theta);		  // log of theta (Beta dispersion parameter)
	PARAMETER(log_sigma);		  // alternative beta dispersion parameter
	PARAMETER(logsd_logdeltar);		   // SDs of {loc, deltar} REs
	PARAMETER(logsd_reinf);		   // SDs of {loc, deltar} REs

	// PARAMETER_VECTOR(corr);	     // correlation among SDs (unused now since only 1 RE per block)

	int nobs = omicron.size();
	// int blocksize = 2; // two parameters per province (unused)
	Type nll;
	Type res = 0;

	if (debug>0) std::cout << "before RE computations\n";

	// black magic inherited from glmmTMB

	// convert RE vector into an array
	// vector<int> dim(2);
	// dim << blocksize, nprov;
	// array<Type> bseg(b, dim);
	// vector<Type> sd = exp(log_sd);
	// // compute RE component of likelihood
	// density::UNSTRUCTURED_CORR_t<Type> nldens(corr);
	// density::VECSCALE_t<density::UNSTRUCTURED_CORR_t<Type> > scnldens = density::VECSCALE(nldens, sd);
	// for(int i = 0; i < nprov; i++) {
	//   res -= scnldens(bseg.col(i));
	//	SIMULATE {
	//		bseg.col(i) = sd * nldens.simulate();
	//	}
	// }

	// random effect term for deltar (b is spherical/unscaled/standard-Normal)
	for (int i=0; i<nprov; i++) {
		res -= dnorm(b_logdeltar(i), Type(0), Type(1), true);
		res -= dnorm(b_reinf(i), Type(0), Type(1), true);
	}

	if (debug>0) std::cout << "after RE computations\n";

	vector<Type> prob(nobs);
	Type s1, s2, s3;

	// calculate probability
	vector<Type> log_deltar_vec = log_deltar + exp(logsd_logdeltar)*b_logdeltar;
	vector<Type> deltar_vec = exp(log_deltar_vec);
	vector<Type> reinf_vec = beta_reinf+ exp(logsd_reinf)*b_reinf;

	for(int i = 0; i < nobs; i++) {
		int j = prov(i);
		if (perfect_tests == 0) {
			prob(i) = baselogis_int(time(i),
						loc(j),
						// deltar includes province-specific REs
						deltar_vec(j),
						lodrop, logain,
						reinf_vec(j)*reinf(i)
						);
		} else {
			prob(i) = invlogit(deltar_vec(j)*(time(i) - loc(j)) + reinf_vec(j)*reinf(i));
		}

		// calculate neg log likelihood
		// FIXME: revert to binomial when theta → ∞ (i.e. over a threshold) ?
		if (notFinite(log_theta) && notFinite(log_sigma)) { 
			nll = -1*dbinom(Type(omicron(i)), Type(tot(i)), prob(i), true);
		} else if (notFinite(log_sigma)) {
			// beta-binomial, theta param
			nll = -1*dbetabinom_theta(Type(omicron(i)), prob(i), exp(log_theta), Type(tot(i)), true);
			SIMULATE {
				omicron(i) = rbetabinom_theta(Type(tot(i)), prob(i), exp(log_theta));
			}
		} else {
			nll = -1*dbetabinom_sigma(Type(omicron(i)), prob(i), exp(log_sigma), Type(tot(i)), true);
			SIMULATE {
				omicron(i) = rbetabinom_sigma(Type(tot(i)), prob(i), exp(log_sigma));
			}
		}

		// copied from glmmTMB: not yet ...
		// s3 = logit_inverse_linkfun(eta(i), link); // logit(p)
		// s1 = log_inverse_linkfun( s3, logit_link) + log(phi(i)); // s1 = log(mu*phi)
		// s2 = log_inverse_linkfun(-s3, logit_link) + log(phi(i)); // s2 = log((1-mu)*phi)
		// tmp_loglik = glmmtmb::dbetabinom_robust(yobs(i), s1, s2, size(i), true);

		if (!notFinite(nll)) res += nll;
		if (debug > 5) {
			std::cout << i << " " << prob(i) << " " << omicron(i) << " " <<
				tot(i) << " " << nll << " " << res << "\n";
		}

	} // loop over observations


	// priors
	Type nlprior = 0;

	if (!notFinite(prior_logsd_logdeltar(0))) {
		nlprior -= 1*dnorm(logsd_logdeltar,
				   prior_logsd_logdeltar(0),
				   prior_logsd_logdeltar(1),
				   true);
	}

	if (!notFinite(prior_logsd_reinf(0))) {
		nlprior -= 1*dnorm(logsd_reinf,
					 prior_logsd_reinf(0),
					 prior_logsd_reinf(1),
				   true);
	}

	res += nlprior;
	
	if (debug > 3) {
		std::cout << "nlprior " << nlprior << " "
			  << logsd_logdeltar << " "
			  << prior_logsd_logdeltar(0) << " "
			  << prior_logsd_logdeltar(1) << " "
			  << "\n";
	}
	
	REPORT(prob);
	REPORT(log_deltar_vec);
	REPORT(reinf_vec);
	SIMULATE{
		REPORT(omicron);
	}
	
	// sdreport() will give both value and sd
	// report log-odds of prob so we can get (Wald) CIs on the logit scale
	vector<Type> loprob = logit(prob);

	ADREPORT(loprob);
	ADREPORT(log_deltar_vec);
	ADREPORT(reinf_vec);

	if (debug > 1) std::cout << res << "\n";
	
	return res;
}
