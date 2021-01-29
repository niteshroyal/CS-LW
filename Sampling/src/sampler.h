/*
 * sampler.h
 *
 * Copyright (C) 2020, 2021 Nitesh Kumar
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef SRC_SAMPLING_H_
#define SRC_SAMPLING_H_

#include <iostream>
#include <vector>
#include "gsl/gsl_rng.h"
#include "gsl/gsl_randist.h"
#include "gsl/gsl_sf_bessel.h"

class sampler {
private:
	gsl_rng *generator;

public:
	/**
	 * Initialize random number generator
	 */
	sampler();

	/**
	 * Free random number generator
	 */
	~sampler();

	/**
	 * Change seed
	 */
	void set_seed(unsigned long int s);

	/**
	 * Sample from a Gaussian distribution with mean m and standard deviation std.
	 */
	double sample_gaussian_dist(double m, double std);

	/**
	 * Get the likelihood weight for a point from Gaussian distribution with mean m and standard deviation std.
	 */
	double weight_gaussian_dist(double m, double std, double x);

	/**
	 * Sample from a bernoulli distribution with parameter p.
	 */
	unsigned int sample_bernoulli_dist(double p);

	/**
	 * Get the likelihood weight for a point from a bernoulli distribution with parameter p.
	 */
	double weight_bernoulli_dist(unsigned int k, double p);

	/**
	 * Sample from a multinomial distribution
	 */
	unsigned int sample_discrete_dist(unsigned int size, double* p);

};

#endif /* SRC_SAMPLING_H_ */
