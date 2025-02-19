"""
A python module for calculating cube root using the 
Newton's method for ME522 lab 2
"""

def cubeNT(x,debug=False,specialCases=True):
	"""
	The actual cubeNTfunction
	Inputs
	x: the number whose cube root is to be calculated
	debug: True if iteration details need to be printed. Default value is False
	specialCases: False to disable zero check. Default value is True

	"""
	if specialCases:
		if x==0.:
			return 0.
	s=1.
	kmax=100
	tol=1.0e-14
	for k in range(kmax):
		if debug:
			print("At iteration number %s, s= %20.15f" %(k,s))
		s0=s
		s = 1/3*(2*s+x/s**2)
		delta_s=s-s0
		if(abs(delta_s/x))<tol:
			break
	if debug:
		print("After %s iterations,  s=%20.15f" %(k+1,s))
	return s

def test_main():
	from numpy import cbrt
	xvalues=[0., 8., 1000, 1.e9]
	for x in xvalues:
		print("Testing with x=%20.15e" %x)
		s=cubeNT(x)
		s_numpy=cbrt(x)
		print("sqrtNT s = %20.15e, numpy s = %20.15e" %(s,s_numpy))
		assert abs(s-s_numpy) < 1e-14, "Your sqrt does not agree with numpy sqrt"
