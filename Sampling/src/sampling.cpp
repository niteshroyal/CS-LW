/*
 * sampling.cpp
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

#include <iostream>
#include <cmath>
#include "SWI-cpp.h"
#include "sampler.h"
#include "stdlib.h"
#include "likelihoodWeighting.h"
#include <string>
#include <iomanip>

using namespace std;

sampler *samplerObj;

struct ListSizeMismatch : public exception {
   const char * what () const throw () {
      return "size of two lists are not same!";
   }
};

struct ProbNotSumToOne : public exception {
   const char * what () const throw () {
      return "discrete: probabilities do not sum to one!";
   }
};

struct ItemNotPresentInList : public exception {
   const char * what () const throw () {
      return "discrete: the item is not present in the list!";
   }
};

void print(std::vector <double> const &a) {
   std::cout << "The vector elements are : ";

   for(int i=0; i < a.size(); i++)
   std::cout << a[i] << ' ';
}

bool double_equals(double a, double b, double epsilon = 0.0001)
{
    return std::abs(a - b) < epsilon;
}

PREDICATE(connect_sampler, 1) {
	int flag = (int)A1;
	if (flag == 1) {
		samplerObj = new sampler();
	} else {
		delete(samplerObj);
	}
	return TRUE;
}

PREDICATE(set_sampler_seed, 1) {
	int seed = (int)A1;
	samplerObj->set_seed(seed);
	return TRUE;
}


PREDICATE(sample_gaussian, 3) {
	double mean = (double)A1;
	double variance = (double)A2;
	return A3 = samplerObj->sample_gaussian_dist(mean, sqrt(variance));
}

PREDICATE(sample_bernoulli, 2) {
	double prob = (double)A1;
	return A2 = (int)samplerObj->sample_bernoulli_dist(prob);
}

PREDICATE(sample_discrete, 2) {
	PlTail tail(A1);
	PlTerm e;
	vector<double> probs;
	double prob;
	double sum_prob = 0;
	while(tail.next(e)) {
		prob = (double)e;
		probs.push_back(prob);
		sum_prob += prob;
	}
	if (!double_equals(sum_prob, 1.0)) {
		// print(probs);
		throw ProbNotSumToOne();
	}
	int nRows = probs.size();
	double* p = &probs[0];
	int sampled_val = samplerObj->sample_discrete_dist(nRows, p);
	return A2 = sampled_val;
}

/*
PREDICATE(sample_discrete, 2) {
	PlTail tail(A1);
	PlTerm e;
	vector<double> probs;
	vector<string> vals;
	double sum_prob = 0;
	while(tail.next(e)) {
		string s = (char*)e;
		vector<string> prob_val = explode(s, ':');
		double this_prob = atof(prob_val[0].c_str());
		sum_prob += this_prob;
		probs.push_back(this_prob);
		vals.push_back(prob_val[1]);
	}
	if (sum_prob != 1.0) {
		throw ProbNotSumToOne();
	}
	int nRows = probs.size();
	double* p = new double[nRows];
	for(int i=0; i<nRows; ++i) {
		p[i] = probs[i];
	}
	unsigned int sampled_val = samplerObj->sample_discrete_dist(nRows, p);
	return A2 = vals[sampled_val].c_str();
}

PREDICATE(weight_discrete, 3) {
	PlTail tail(A1);
	PlTerm e;
	string compare_val = (char*)A2;
	double weight = -1.0;
	double sum_prob = 0;
	while(tail.next(e)) {
		string s = (char*)e;
		vector<string> prob_val = explode(s, ':');
		double this_prob = atof(prob_val[0].c_str());
		if (compare_val == prob_val[1]) {
			weight = this_prob;
		}
		sum_prob += this_prob;
	}
	if (weight == -1.0) {
		throw ItemNotPresentInList();
	}
	if (sum_prob != 1.0) {
		throw ProbNotSumToOne();
	}
	return A3 = weight;
}
*/

PREDICATE(expected_lw, 2) {
	PlTail tail(A1);
	PlTerm e;
	vector<vector<double>> input;
	while(tail.next(e)) {
		vector<double> inner;
		PlTail tailIn(e);
		PlTerm eIn;
		while(tailIn.next(eIn)) {
			inner.push_back((double)eIn);
		}
		input.push_back(inner);
	}
	int rows = input.size();
	double* result = expectedLW(input, 1); // 0: Traditional way, 1: Non-Traditional
	PlTermv av(1);
	PlTail l(av[0]);
	for(int i=0; i<rows; ++i) {
		l.append(result[i]);
	}
	l.close();
	return A2 = av[0];
}



PREDICATE(expected_lw, 3) {
	PlTail tail(A1);
	PlTerm e;
	vector<vector<double>> input1;
	while(tail.next(e)) {
		vector<double> inner;
		PlTail tailIn(e);
		PlTerm eIn;
		while(tailIn.next(eIn)) {
			inner.push_back((double)eIn);
		}
		input1.push_back(inner);
	}
	PlTail tail2(A2);
	PlTerm e2;
	vector<vector<int>> input2;
	while(tail2.next(e2)) {
		vector<int> inner;
		PlTail tailIn(e2);
		PlTerm eIn;
		while(tailIn.next(eIn)) {
			inner.push_back((int)eIn);
		}
		input2.push_back(inner);
	}
	int rows = input1.size();
	if(rows != input2.size()) {
		throw ListSizeMismatch();
	} else {
	}
	double* result = expectedLW(input1, input2);
	PlTermv av(1);
	PlTail l(av[0]);
	for(int i=0; i<rows; ++i) {
		l.append(result[i]);
	}
	l.close();
	return A3 = av[0];
}




PREDICATE(compute_prob, 3) {
	PlTail tail1(A1);
	PlTerm e;
	vector<int> entail;
	while(tail1.next(e)) {
		entail.push_back((int)e);
	}
	PlTail tail2(A2);
	PlTerm f;
	vector<double> expectedLW;
	while(tail2.next(f)) {
		expectedLW.push_back((double)f);
	}
	if(entail.size() != expectedLW.size()) {
		throw ListSizeMismatch();
	}
	double nume = 0;
	double deno = 0;
	for(int i=0; i<expectedLW.size(); i++) {
		if(entail[i] == 1) {
			nume += expectedLW[i];
			deno += expectedLW[i];
		} else {
			nume += 0;
			deno += expectedLW[i];
		}
	}
	return A3 = nume/deno;
}


/*
PREDICATE(send_list, 1) {
	//double arr[3][4] = {{2.0, 3.9, 9.0, 2.9}, {4.0, 3.6, 3.0, 1.9}, {8.0, 6.9, 8.0, 2.9}};

	double arr[1][1] = {{3.5}};

	PlTermv av(1);
	PlTail l(av[0]);
	for(int i=0; i<1; i++) {
		PlTermv avIn(1);
		PlTail lIn(avIn[0]);
		for(int j=0; j<1; j++) {
			lIn.append(arr[i][j]);
		}
		lIn.close();
		l.append(avIn[0]);
	}
	l.close();
	return A1 = av[0];
}
*/







