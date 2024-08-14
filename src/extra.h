#ifndef _EXTRA_H_
#define _EXTRA_H_

//-*-c++-*-
// This file contains a few useful things
namespace vpdtw {

	// A function to quickly compute the square of something
	inline double sqr(const double x) {return(x*x);}

	// A function to quickly compute the absolute value of something
	inline int abs(const int x) {return((x<0)?(-x):x);}
	inline double abs(const double x) {return((x<0)?(-x):x);}

	// A function to find the maximum of two inputs
	inline int max(const int x, const int y) {return((x<y)?y:x);}
	inline double max(const double x, const double y) {return((x<y)?y:x);}
	inline int min(const int x, const int y) {return((x<y)?x:y);}
	inline double min(const double x, const double y) {return((x<y)?x:y);}
}

#endif
