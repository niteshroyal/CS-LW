/*
 * sampler.cpp
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

#include "sampler.h"
#include <ctime>

using namespace std;

sampler::sampler() {
	const gsl_rng_type * t;
	gsl_rng_env_setup();
	t = gsl_rng_default;
	generator = gsl_rng_alloc(t);
}

void sampler::set_seed(unsigned long int s) {
	gsl_rng_set(generator, s);
}

sampler::~sampler() {
	gsl_rng_free(generator);
}

double sampler::sample_gaussian_dist(double m, double std) {
	double s = gsl_ran_gaussian(generator, std);
	return s+m;
}

double sampler::weight_gaussian_dist(double m, double std, double x) {
	x = x-m;
	return gsl_ran_gaussian_pdf(x, std);
}

unsigned int sampler::sample_discrete_dist(unsigned int size, double* p) {
	gsl_ran_discrete_t* temp =  gsl_ran_discrete_preproc(size, p);
	return gsl_ran_discrete(generator, temp);
}

unsigned int sampler::sample_bernoulli_dist(double p) {
	return gsl_ran_bernoulli(generator, p);
}

double sampler::weight_bernoulli_dist(unsigned int k, double p) {
	return gsl_ran_bernoulli_pdf(k, p);
}


int main() {
	int i;
	sampler *samplerObj = new sampler();
	int x;
	clock_t begin = clock();
	for(i=0; i<10; i++) {
		x = (int)samplerObj->sample_bernoulli_dist(0.9);
	}
	clock_t end = clock();
	double elapsed_secs = double(end - begin) / CLOCKS_PER_SEC;
	cout << "Time elapsed in secs: " << elapsed_secs <<"\n";
	return 0;
}
